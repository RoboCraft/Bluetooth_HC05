(defmacro kw (arg)
  `(span :class "keyword" ,arg))

(defmacro op (arg)
  `(span :class "operator" ,arg))

(defmacro const (arg)
  `(span :class "constant" ,arg))

(defmacro num (arg)
  `(span :class "number" ,arg))

(defmacro line (&body body)
  `(div ,@body (br)))

(defmacro lines (&body body)
  `(progn
    ,@(loop for ln in body collecting `(line ,ln))))

(defmacro desc (&body body)
  `(blockquote (pre ,@body)))

(defconstant +INDENT+ "  ")
(defmacro >> () `(format t +INDENT+))

(html
  (head
    (title +TITLE+)
    (meta :http-equiv "Content-Type" :content "text/html" :charset "utf-8")
    
    (style :type "text/css"
      (cssexp:with-css-output (*standard-output*)
        (:.keyword :color "#00007f" :font-weight "bold")
        (:.operator :color "#301010")
        (:.constant :color "#663300")
        (:.number :color "#007f00")
        (:blockquote :margin "1em 1em" :border-left "2px solid #999" :padding-left "1em")
        )));([blockquote p] :margin 0))))
        
        ;~ blockquote {
	;~ margin: 1em 3em;
	;~ color: #999;
	;~ border-left: 2px solid #999;
	;~ padding-left: 1em; }

  (body
    (h1 +TOP-HEADER+)
    (p +SYNOPSIS+)
    (h2 +DATATYPES-HEADER+)
    (h2 +CONSTANTS-HEADER+)
    
    (const "HC05_DEFAULT_TIMEOUT")
    (desc "dsdsds")
      
    ;(pre
      (lines
        ;(kw "enum")
        ;(op "{")
        (progn (>>) (const "HC05_DEFAULT_TIMEOUT") (op " = ") (num 200))
        (progn (>>) (const "HC05_INQUIRY_DEFAULT_TIMEOUT") (op " = ") (num 10000))
        (progn (>>) (const "HC05_PAIRING_DEFAULT_TIMEOUT") (op " = ") (num 10000))
        (progn (>>) (const "HC05_PASSWORD_MAXLEN") (op " = ") (num 16))
        (progn (>>) (const "HC05_PASSWORD_BUFSIZE") (op " = ") (const "HC05_PASSWORD_MAXLEN") (op " + ") (num 1))
        (progn (>>) (const "HC05_NAME_MAXLEN") (op " = ") (num 32))
        (progn (>>) (const "HC05_NAME_BUFSIZE") (op " = ") (const "HC05_NAME_MAXLEN") (op " + ") (num 1))
        (progn (>>) (const "HC05_ADDRESS_MAXLEN") (op " = ") (num 14))
        (progn (>>) (const "HC05_ADDRESS_BUFSIZE") (op " = ") (const "HC05_ADDRESS_BUFSIZE") (op " + ") (num 1))
        ;(op "};")
        )));)
    
;~ &nbsp;&nbsp;<span class="constant">HC05_DEFAULT_TIMEOUT&nbsp;</span><span class="operator">=&nbsp;</span><span class="number">200</span><span class="operator">,</span><br />
;~ &nbsp;&nbsp;<span class="constant">HC05_INQUIRY_DEFAULT_TIMEOUT&nbsp;</span><span class="operator">=&nbsp;</span><span class="number">10000</span><span class="operator">,</span><br />
;~ &nbsp;&nbsp;<span class="constant">HC05_PAIRING_DEFAULT_TIMEOUT&nbsp;</span><span class="operator">=&nbsp;</span><span class="number">10000</span><span class="operator">,</span><br />
;~ &nbsp;&nbsp;<span class="constant">HC05_PASSWORD_MAXLEN&nbsp;</span><span class="operator">=&nbsp;</span><span class="number">16</span><span class="operator">,</span><br />
;~ &nbsp;&nbsp;<span class="constant">HC05_PASSWORD_BUFSIZE&nbsp;</span><span class="operator">=&nbsp;</span><span class="constant">HC05_PASSWORD_MAXLEN&nbsp;</span><span class="operator">+&nbsp;</span><span class="number">1</span><span class="operator">,</span><br />
;~ &nbsp;&nbsp;<span class="constant">HC05_NAME_MAXLEN&nbsp;</span><span class="operator">=&nbsp;</span><span class="number">32</span><span class="operator">,</span><br />
;~ &nbsp;&nbsp;<span class="constant">HC05_NAME_BUFSIZE&nbsp;</span><span class="operator">=&nbsp;</span><span class="constant">HC05_NAME_MAXLEN&nbsp;</span><span class="operator">+&nbsp;</span><span class="number">1</span><span class="operator">,</span><br />
;~ &nbsp;&nbsp;<span class="constant">HC05_ADDRESS_MAXLEN&nbsp;</span><span class="operator">=&nbsp;</span><span class="number">14</span><span class="operator">,</span><br />
;~ &nbsp;&nbsp;<span class="constant">HC05_ADDRESS_BUFSIZE&nbsp;</span><span class="operator">=&nbsp;</span><span class="constant">HC05_ADDRESS_MAXLEN&nbsp;</span><span class="operator">+&nbsp;</span><span class="number">1</span><span class="operator">,</span><br />

