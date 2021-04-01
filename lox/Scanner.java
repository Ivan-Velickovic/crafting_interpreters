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
        keywords.put("and",    AND);
        keywords.put("class",  CLASS);
        keywords.put("else",   ELSE);
        keywords.put("false",  FALSE);
        keywords.put("for",    FOR);
        keywords.put("fn",     FUNCTION);
        keywords.put("if",     IF);
        keywords.put("nil",    NIL);
        keywords.put("or",     OR);
        keywords.put("print",  PRINT);
        keywords.put("return", RETURN);
        keywords.put("super",  SUPER);
        keywords.put("this",   THIS);
        keywords.put("true",   TRUE);
        keywords.put("var",    VAR);
        keywords.put("while",  WHILE);
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
        tokens.add(new Token(EOF, "", null, line));

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
            case '(': addToken(LEFT_PAREN);  break;
            case ')': addToken(RIGHT_PAREN); break;
            case '{': addToken(LEFT_BRACE);  break;
            case '}': addToken(RIGHT_BRACE); break;
            case ',': addToken(COMMA);       break;
            case '.': addToken(DOT);         break;
            case '-': addToken(MINUS);       break;
            case '+': addToken(PLUS);        break;
            case ';': addToken(SEMICOLON);   break;
            case '*': addToken(STAR);        break;
            case '!':
                addToken(match('=') ? BANG_EQUAL : BANG);
                break;
            case '=':
                addToken(match('=') ? EQUAL_EQUAL : EQUAL);
                break;
            case '<':
                addToken(match('=') ? LESS_EQUAL : LESS);
                break;
            case '>':
                addToken(match('=') ? GREATER_EQUAL : GREATER);
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
            addToken(SLASH);
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
        addToken(STRING, value);
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

        addToken(NUMBER, Double.parseDouble(source.substring(start, current)));
    }

    private void scanIdentifier() {
        while (isAlphaNumeric(peek())) {
            advance();
        }

        String text = source.substring(start, current);
        TokenType type = keywords.get(text);
        if (type == null) {
            type = IDENTIFIER;
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
