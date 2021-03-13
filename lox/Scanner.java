package lox;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static lox.TokenType.*; // TODO: probably shouldn't .* but Java seems not willing to let me just import the enum

class Scanner {
    private final String source;
    private final List<Token> tokens = new ArrayList<>();

    private int start = 0;   // First char of lexeme
    private int current = 0; // Offset from start to current char
    private int line = 1;

    private static final Map<String, TokenType> keywords;

    static {
        keywords = new HashMap<>();
        keywords.put("and",    TokenType.AND);
        keywords.put("class",  TokenType.CLASS);
        keywords.put("else",   TokenType.ELSE);
        keywords.put("false",  TokenType.FALSE);
        keywords.put("for",    TokenType.FOR);
        keywords.put("fun",    TokenType.FUN);
        keywords.put("if",     TokenType.IF);
        keywords.put("nil",    TokenType.NIL);
        keywords.put("or",     TokenType.OR);
        keywords.put("print",  TokenType.PRINT);
        keywords.put("return", TokenType.RETURN);
        keywords.put("super",  TokenType.SUPER);
        keywords.put("this",   TokenType.THIS);
        keywords.put("true",   TokenType.TRUE);
        keywords.put("var",    TokenType.VAR);
        keywords.put("while",  TokenType.WHILE);
    }

    Scanner(String source) {
        this.source = source;
    }

    List<Token> scanTokens() {
        // Continously add tokens until we reach the end of file.
        while(!consumedAllChars()) {
            start = current;
            scanToken();
        }
        tokens.add(new Token(TokenType.EOF, "", null, line));

        return tokens;
    }

    private boolean consumedAllChars() {
        return current >= source.length();
    }

    private char advance() {
        return source.charAt(current++);
    }

    private void addToken(TokenType type) {
        addToken(type, null);
    }

    private void addToken(TokenType type, Object literal) {
        String text = source.substring(start, current);
        tokens.add(new Token(type, text, literal, line));
    }

    private void scanToken() {
        char c = advance();
        switch (c) {
            case '(': addToken(TokenType.LEFT_PAREN);  break;
            case ')': addToken(TokenType.RIGHT_PAREN); break;
            case '{': addToken(TokenType.LEFT_BRACE);  break;
            case '}': addToken(TokenType.RIGHT_BRACE); break;
            case ',': addToken(TokenType.COMMA);       break;
            case '.': addToken(TokenType.DOT);         break;
            case '-': addToken(TokenType.MINUS);       break;
            case '+': addToken(TokenType.PLUS);        break;
            case ';': addToken(TokenType.SEMICOLON);   break;
            case '*': addToken(TokenType.STAR);        break;
            case '!':
                addToken(match('=') ? TokenType.BANG_EQUAL : TokenType.BANG);
                break;
            case '=':
                addToken(match('=') ? TokenType.EQUAL_EQUAL : TokenType.EQUAL);
                break;
            case '<':
                addToken(match('=') ? TokenType.LESS_EQUAL : TokenType.LESS);
                break;
            case '>':
                addToken(match('=') ? TokenType.GREATER_EQUAL : TokenType.GREATER);
                break;
            case '/': skipLineComment(); break;

            case ' ':
            case '\r':
            case '\t':
            // Ignore whitespace
                break;

            case '\n':
                line++;
                break;

            case '"': scanString(); break;

            default:
                if (isDigit(c)) {
                    scanNumber();
                } else if (isAlpha(c)) {
                    scanIdentifier();
                } else {
                    // TODO: continously look at further illegal tokens and bunch them into one lexical error
                    Lox.error(line, "Unexpected character: " + c);
                }
        }
    }

    private void skipLineComment() {
        // Assumes current char is '/'
        if (match('/')) {
            while (peek() != '\n' && !consumedAllChars()) {
                advance();
            }
        } else {
            addToken(TokenType.SLASH);
        }
    }

    private void scanString() {
        while (peek() != '"' && !consumedAllChars()) {
            if (peek() == '\n') line++;
            advance();
        }

        if (consumedAllChars()) {
            // Must have reached EOF before closing quote
            Lox.error(line, "Unterminated string.");
        }

        advance(); // Accepting closing '"'.

        // Trim surrounding quotes,
        // NOTE: Lox doesn't support doesn't support escape sequences, possible TODO
        String value = source.substring(start + 1, current - 1);
        addToken(TokenType.STRING, value);
    }

    private void scanNumber() {
        while (isDigit(peek())) {
            advance();
        }

        // Look for fractional part.
        if (peek() == '.' && isDigit(peekNext())) {
            advance(); // Conusmes '.'

            while (isDigit(peek())) {
                advance();
            }
        }

        addToken(TokenType.NUMBER, Double.parseDouble(source.substring(start, current)));
    }

    private void scanIdentifier() {
        while (isAlphaNumeric(peek())) {
            advance();
        }

        String text = source.substring(start, current);
        TokenType type = keywords.get(text);
        if (type == null) {
            type = TokenType.IDENTIFIER;
        }
        addToken(type);
    }

    private boolean match(char expected) {
        if (consumedAllChars()) return false;
        if (source.charAt(current) != expected) return false;

        current++;
        return true;
    }

    private char peek() {
        if (consumedAllChars()) return '\0';
        return source.charAt(current);
    }

    private char peekNext() {
        if (current + 1 >= source.length()) return '\0';
        return source.charAt(current + 1);
    }

    private boolean isDigit(char c) {
        return c >= '0' && c <= '9';
    }

    private boolean isAlpha(char c) {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
    }

    private boolean isAlphaNumeric(char c) {
        return isAlpha(c) || isDigit(c);
    }

}
