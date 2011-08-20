(constants
  (+title+
    "Библиотека Bluetooth_HC05")
  (+top-header+
    "Описание библиотеки Bluetooth_HC05")
  (+synopsis+
    "Библиотека предназначена для работы с Bluetooth-модулем HC-05 и реализует
    все 36 функций, описанных в документации (datasheet) к модулю от компании Wavesen,
    в виде класса Bluetooth_HC05 и его методов.
    Почти все методы принимают в качестве последнего агрумента тайм-аут на чтение ответа
    от модуля (в миллисекундах). К класс Bluetooth_HC05 реализует интерфейс Print
    так что вы можете посылать данные, пользуясь методом print(), как в объект класса Serial.")
  
  (+table-of-contents-header+ "Оглавление")
  
  (+datatypes-header+ "Типы данных")
  (+constants-header+ "Константы")
  
  (+HC05_DEFAULT_TIMEOUT-description+
    "Тайм-аут по умолчанию для большинства команд.")
  (+HC05_INQUIRY_DEFAULT_TIMEOUT-description+
    "Тайм-аут по умолчанию для опроса (inquiring) Bluetooth-устройств.")
  (+HC05_PAIRING_DEFAULT_TIMEOUT-description+
    "Тайм-аут по умолчанию для образования пары (pairing) между устройствами.")
  (+HC05_PASSWORD_MAXLEN-description+
    "Максимальная длина пароля (PIN-кода), которую позволяет задать модуль.")
  (+HC05_PASSWORD_BUFSIZE-description+
    "Размер буфера, способного вместить самый длинный пароль.")
  (+HC05_NAME_MAXLEN-description+ "Максимальная длина имени устройства.")
  (+HC05_NAME_BUFSIZE-description+ "Размер буфера для имени устройства.")
  (+HC05_ADDRESS_MAXLEN-description+
    "Максимальная длина строкового представления адреса Bluetooth-устройства.")
  (+HC05_ADDRESS_BUFSIZE-description+
    "Размер буфера для строкового представления Bluetooth-адреса.")
  
  (+HC05_Mode-description+ "Режим работы модуля")
  (+HC05_MODE_DATA-description+ "приём-передача данных")
  (+HC05_MODE_COMMAND-description+ "управление AT-командами")
  
  (+HC05_Role-description+ "Роль Bluetooth-модуля")
  (+HC05_ROLE_SLAVE-description+ "Пассивное подключение (к модулю можно подключаться)")
  (+HC05_ROLE_MASTER-description+
    "Активное подключение (модуль сам подключается к устройствам)")
  (+HC05_ROLE_SLAVE_LOOP-description+
    "Пассивное подключение с эхом: все принятые данные передаются обратно")
  
  (+HC05_InquiryMode-description+ "Режим опроса устройств")
  (+HC05_INQUIRY_STANDARD-description+ "Обычный (все устройства)")
  (+HC05_INQUIRY_RSSI-description+ "Только устройства с ~a")
  (+RSSI-wikipedia-link+
    '("RSSI" . "http://ru.wikipedia.org/wiki/Received_Signal_Strength_Indication"))
  
  (+HC05_Parity-description+ "Использование ~a")
  (+HC05_NO_PARITY-description+ "Без бита чётности")
  (+HC05_PARITY_ODD-description+ "Нечётный")
  (+HC05_PARITY_EVEN-description+ "Чётный")
  (+parity-bit-wikipedia-link+
    '("бита чётности" . "http://ru.wikipedia.org/wiki/Бит_чётности"))
  
  (+HC05_Connection-description+ "Режим соединения")
  (+bind-method-name+ "bind()")
  (+HC05_CONNECT_BOUND-description+
    "Принимать соединение только от устройства с определённым адресом,
    задаваемым методом ~a")
  (+HC05_CONNECT_ANY-description+ "")
  (+HC05_CONNECT_SLAVE_LOOP-description+ "")
  
  ;~ 0----connect the module to the
  ;~ specified Bluetooth address.
  ;~ (Bluetooth
  ;~ specified
  ;~ address
  ;~ by
  ;~ the
  ;~ can
  ;~ be
  ;~ binding
  ;~ command)
  ;~ + CMODE:<Param>
  ;~ 1----connect the module to any
  ;~ AT+ CMODE?
  ;~ OK
  ;~ address
  ;~ (The specifying address has no
  ;~ effect for this mode.)
  ;~ 2----Slave-Loop
  ;~ Default connection mode: 0

    
  (+HC05_Security-description+ "")
  (+HC05_SEC_OFF-description+ "")
  (+HC05_SEC_NON_SECURE-description+ "")
  (+HC05_SEC_SERVICE-description+ "")
  (+HC05_SEC_LINK-description+ "")
  (+HC05_SEC_UNKNOWN-description+ "")
    
  (+HC05_Encryption-description+ "")
  (+HC05_ENC_OFF-description+ "")
  (+HC05_ENC_PTP-description+ "")
  (+HC05_ENC_PTP_BROADCAST-description+ "")
    
  (+HC05_State-description+ "")
  (+HC05_INITIALIZED-description+ "")
  (+HC05_READY-description+ "")
  (+HC05_PAIRABLE-description+ "")
  (+HC05_PAIRED-description+ "")
  (+HC05_INQUIRING-description+ "")
  (+HC05_CONNECTING-description+ "")
  (+HC05_CONNECTED-description+ "")
  (+HC05_DISCONNECTED-description+ "")
  (+HC05_UNKNOWN-description+ "")
  )
