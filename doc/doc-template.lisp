(defmacro keyword-style (arg)
  `(span :class "keyword" ,arg))

(defmacro operator-style (arg)
  `(span :class "operator" ,arg))

(defmacro const-style (arg)
  `(span :class "constant" ,arg))

(defmacro num-style (arg)
  `(span :class "number" ,arg))

(defmacro type-style (arg)
  `(span :class "type" ,arg))

(defmacro method-style (arg)
  `(span :class "method" ,arg))

(defmacro chinese-warning ()
  `(span :class "chinese-warning" "狗屁"))

(defmacro line (&body body)
  `(progn ,@body (br)))

(defmacro lines (&body body)
  `(progn
    ,@(loop for ln in body collecting `(line ,ln))))

(defmacro desc (&body body)
  `(line (blockquote ,@body)))

(defconstant +indent+ "  ")
(defmacro >> () `(format t +indent+))

(defmacro hlink (address text)
  `(lml-format "<a href=\"~a\">~a</a>" ,address ,text))

(defmacro hlink-from-pair (pair)
  `(hlink (cdr ,pair) (car ,pair)))

(defmacro description-with-link (description link-pair)
 `(html-string
    (lml-format ,description
      (html-string (hlink-from-pair ,link-pair)))))

(defmacro section (name &body body)
  `(h2 (a :name ,name ,@body)))

(defmacro section-link (name prefix &body body)
  `(line (lml-princ ,prefix) (hlink (string-concat "#" ,name) ,@body)))

(defmacro const-description (name description)
 `(p (const-style ,name) (br) ,description))

(defmacro const-descriptions (&rest pairs)
 `(progn
  ,@(loop for pair in pairs collecting
     `(const-description ,(first pair) ,(second pair)))))

(defmacro enum-description (name description &body body)
 `(p(div :class "enum-description"
    (line (type-style ,name))
    (princ ,description)
    (blockquote
      (const-descriptions ,@body)))))

(defmacro css (&body body)
  `(with-output-to-string (css-output)
    (cssexp:with-css-output (css-output)
      ,@body)))

(defmacro style (&rest args)
  `(with style ,@args))

(defmacro html-string (&body body)
 `(with-output-to-string (*html-output*) ,@body))

(defconstants
  (+xml-prologue-string+ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>")
  (+xhtml-prologue-string+ "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml10-strict.dtd\">"))

(lml-format "~A~%" +xml-prologue-string+)
(lml-format "~A~%" +xhtml-prologue-string+)
(html :xmlns "http://www.w3.org/1999/xhtml"
  (head
    (title +title+)
    (meta :http-equiv "Content-Type" :content "text/html" :charset "utf-8")

    (style :type "text/css"
      (cssexp:with-css-output (*standard-output*)
        (:.keyword
          :color "#00007f"
          :font-weight "bold")
        (:.operator :color "#301010")
        (:.constant :color "#663300")
        (:.number :color "#007f00")
        (:.type
          :color "#2b71fd"
          :font-weight "bold")
        (:.method
          :color "#555533"
          :font-weight "bold"
          :font-style "italic"
          );:font-weight "bold")
        (:.enum-description
          :border "2px solid #aaaaaa"
          :padding "0.5em")
        (:.chinese-warning
          :margin "5px 5px"
          :border "2px solid #ff4400"))))

  (body
    (h1 +top-header+)
    (p (lml-format +synopsis+ (html-string (chinese-warning))))

    ;; Table of contents
    (section-link "datatypes" "1. " +datatypes-header+)
    (section-link "constants" "2. " +constants-header+)

    (section "datatypes" "1. " +datatypes-header+)
    (section "constants" "2. " +constants-header+)

    (const-descriptions
      ("HC05_DEFAULT_TIMEOUT" +HC05_DEFAULT_TIMEOUT-description+)
      ("HC05_INQUIRY_DEFAULT_TIMEOUT" +HC05_INQUIRY_DEFAULT_TIMEOUT-description+)
      ("HC05_PAIRING_DEFAULT_TIMEOUT" +HC05_PAIRING_DEFAULT_TIMEOUT-description+)
      ("HC05_PASSWORD_MAXLEN" +HC05_PASSWORD_MAXLEN-description+)
      ("HC05_PASSWORD_BUFSIZE" +HC05_PASSWORD_BUFSIZE-description+)
      ("HC05_NAME_MAXLEN" +HC05_NAME_MAXLEN-description+)
      ("HC05_NAME_BUFSIZE" +HC05_NAME_BUFSIZE-description+)
      ("HC05_ADDRESS_MAXLEN" +HC05_ADDRESS_MAXLEN-description+)
      ("HC05_ADDRESS_BUFSIZE" +HC05_ADDRESS_BUFSIZE-description+))

    (enum-description "HC05_Mode" +HC05_Mode-description+
      ("HC05_MODE_DATA" +HC05_MODE_DATA-description+)
      ("HC05_MODE_COMMAND" +HC05_MODE_COMMAND-description+))

    (enum-description "HC05_Role" +HC05_Role-description+
      ("HC05_ROLE_SLAVE" +HC05_ROLE_SLAVE-description+)
      ("HC05_ROLE_MASTER" +HC05_ROLE_MASTER-description+)
      ("HC05_ROLE_SLAVE_LOOP" +HC05_ROLE_SLAVE_LOOP-description+))

    (enum-description "HC05_InquiryMode" +HC05_InquiryMode-description+
      ("HC05_INQUIRY_STANDARD" +HC05_INQUIRY_STANDARD-description+)
      ("HC05_INQUIRY_RSSI"
        (lml-format +HC05_INQUIRY_RSSI-description+
           (html-string (hlink-from-pair +RSSI-wikipedia-link+)))))

    (enum-description "HC05_Parity"
      (description-with-link +HC05_Parity-description+ +parity-bit-wikipedia-link+)
      ("HC05_NO_PARITY" +HC05_NO_PARITY-description+)
      ("HC05_PARITY_ODD" +HC05_PARITY_ODD-description+)
      ("HC05_PARITY_EVEN" +HC05_PARITY_EVEN-description+))

    (enum-description "HC05_Connection" +HC05_Connection-description+
      ("HC05_CONNECT_BOUND"
        (lml-format +HC05_CONNECT_BOUND-description+
          (html-string (method-style +bind-method-name+))))
      ("HC05_CONNECT_ANY" +HC05_CONNECT_ANY-description+)
      ("HC05_CONNECT_SLAVE_LOOP" (span +HC05_CONNECT_SLAVE_LOOP-description+
                                       (chinese-warning))))

    (enum-description "HC05_Security"
      (description-with-link +HC05_Security-description+ +security-link+)
      ("HC05_SEC_OFF" (span +HC05_SEC_OFF-description+ (chinese-warning)))
      ("HC05_SEC_NON_SECURE" +HC05_SEC_NON_SECURE-description+)
      ("HC05_SEC_SERVICE" +HC05_SEC_SERVICE-description+)
      ("HC05_SEC_LINK" +HC05_SEC_LINK-description+)
      ("HC05_SEC_UNKNOWN" (span +HC05_SEC_UNKNOWN-description+ (chinese-warning))))

    (enum-description "HC05_Encryption"
      (html-string
        (lml-format +HC05_Encryption-description+
          (html-string (hlink-from-pair +security-link+))
          (html-string (b +packet-encryption-section+))))
      ("HC05_ENC_OFF" +HC05_ENC_OFF-description+)
      ("HC05_ENC_PTP" +HC05_ENC_PTP-description+)
      ("HC05_ENC_PTP_BROADCAST" +HC05_ENC_PTP_BROADCAST-description+))

    (enum-description "HC05_State" +HC05_State-description+
      ("HC05_INITIALIZED" +HC05_INITIALIZED-description+)
      ("HC05_READY" (span +HC05_READY-description+ (chinese-warning)))
      ("HC05_PAIRABLE" +HC05_PAIRABLE-description+)
      ("HC05_PAIRED" +HC05_PAIRED-description+)
      ("HC05_INQUIRING" +HC05_INQUIRING-description+)
      ("HC05_CONNECTING" +HC05_CONNECTING-description+)
      ("HC05_CONNECTED" +HC05_CONNECTED-description+)
      ("HC05_DISCONNECTED" +HC05_DISCONNECTED-description+)
      ("HC05_UNKNOWN" (span +HC05_UNKNOWN-description+ (chinese-warning))))))
