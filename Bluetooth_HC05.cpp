#include <WProgram.h>
#include <stdio.h>
#include <ctype.h>
#include "LiquidCrystalExt.h"
#include "Bluetooth_HC05.h"


const float HC05_INQUIRY_QUANT = 1.28f;
//extern LiquidCrystal lcd;

typedef int8_t (*NumCharDecoder)(char c);


int8_t dec_value(char c)
{
  return (c >= '0' && c <= '9') ? (c - '0') : -1;
}


int8_t hex_value(char c)
{
  if (c >= '0' && c <= '9')
    return (c - '0');
  else if (c >= 'a' && c <= 'f')
    return (c - 'a') + 10;
  else if (c >= 'A' && c <= 'F')
    return (c - 'A') + 10;
  
  return -1;
}


template <typename T>
struct IsSigned
{
  enum { result = ((T)-1) < 0 };
};


template <typename ResultType, uint8_t base, NumCharDecoder decode>
struct NumParser
{
  static ResultType parse(const char *&str)
  {
    if (!str || !str[0])
      return 0;
    
    char sign = *str;
    
    if (sign == '+' || sign == '-')
    {
      if (IsSigned<ResultType>::result)
        ++str;
      else
        return 0;
    }
        
    ResultType num = 0;
    int8_t char_value;
    
    while ((char_value = decode(*str)) != -1)
    {
      num *= base;
      num += char_value;
      ++str;
    }
    
    if (sign == '-')
      num = -num;
    
    return num;
  }
};


typedef NumParser<uint8_t, 10u, dec_value> dec_u8;
typedef NumParser<uint16_t, 10u, dec_value> dec_u16;
typedef NumParser<uint32_t, 10u, dec_value> dec_u32;
typedef NumParser<uint8_t, 16u, hex_value> hex_u8;
typedef NumParser<uint16_t, 16u, hex_value> hex_u16;
typedef NumParser<uint32_t, 16u, hex_value> hex_u32;
typedef NumParser<int8_t, 10, dec_value> dec_i8;
typedef NumParser<int16_t, 10, dec_value> dec_i16;
typedef NumParser<int32_t, 10, dec_value> dec_i32;
typedef NumParser<int8_t, 16, hex_value> hex_i8;
typedef NumParser<int16_t, 16, hex_value> hex_i16;
typedef NumParser<int32_t, 16, hex_value> hex_i32;


Bluetooth_HC05::Bluetooth_HC05(HardwareSerial &serial):
  m_uart(&serial), m_timeout(HC05_DEFAULT_TIMEOUT), m_ticksAtStart(millis()),
  m_modePin(0xFF), m_resetPin(0xFF), m_errorCode(HC05_OK)
{
}


Bluetooth_HC05::~Bluetooth_HC05()
{
}


HC05_Result Bluetooth_HC05::getLastError() const
{
  return m_errorCode;
}


void Bluetooth_HC05::begin(unsigned baud_rate, uint8_t reset_pin,
  uint8_t mode_pin, HC05_Mode mode)
{
  m_uart->begin(baud_rate);
  
  m_resetPin = reset_pin;
  pinMode(m_resetPin, OUTPUT);
  digitalWrite(m_resetPin, HIGH);
  
  m_modePin = mode_pin;
  pinMode(m_modePin, OUTPUT);
  digitalWrite(m_modePin, (mode == HC05_MODE_DATA ? LOW : HIGH));
  
  hardReset();
}


bool Bluetooth_HC05::probe(unsigned long timeout)
{
  startOperation(timeout);
  writeCommand(0);
  return readOperationResult();
}


void Bluetooth_HC05::hardReset()
{
  digitalWrite(m_resetPin, LOW);
  delay(6);
  digitalWrite(m_resetPin, HIGH);
  
  m_errorCode = HC05_OK;
}


bool Bluetooth_HC05::softReset(unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("RESET");
  
  return readOperationResult();
}


bool Bluetooth_HC05::getVersion(char *buffer, size_t buffer_size, unsigned long timeout)
{
  startOperation(timeout);
  
  if (!buffer || buffer_size <= 1)
    return false;
  
  writeCommand("VERSION?");
  
  /* Response should look like "+VERSION:2.0-20100601" */
  char response[30];
  const char *version = readResponseWithPrefix(response, sizeof(response), "+VERSION:");
  
  if (m_errorCode != HC05_OK)
    return false;
  
  if (!version)
  {
    *buffer = 0;
    return readOperationResult() && false;
  }
  
  size_t copy_size = min(strlen(version), buffer_size - 1);
  memcpy(buffer, version, copy_size);
  buffer[copy_size] = 0;
  
  return readOperationResult();
}


bool Bluetooth_HC05::restoreDefaults(unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("ORGL");
  return readOperationResult();
}


bool Bluetooth_HC05::getAddress(BluetoothAddress &address, unsigned long timeout)
{
  return readAddressWithCommand(address, "ADDR", timeout);
}


bool Bluetooth_HC05::getName(char *buffer, size_t buffer_size, unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("NAME?");
  
  char response[40];
  char *name = readResponseWithPrefix(response, sizeof(response), "+NAME:");
  
  if (m_errorCode != HC05_OK)
    return false;
  
  if (!name)
  {
    *buffer = 0;
    return readOperationResult() && false;
  }
  
  size_t copy_size = min(strlen(name), buffer_size - 1);
  memcpy(buffer, name, copy_size);
  buffer[copy_size] = 0;
  
  return readOperationResult();
}


bool Bluetooth_HC05::setName(const char *name, unsigned long timeout)
{
  startOperation(timeout);
  
  if (!name || name[0] == 0)
    return false;
  
  writeCommand("NAME=", name);
  return readOperationResult();
}


bool Bluetooth_HC05::getRemoteDeviceName(char *buffer,
  size_t buffer_size, unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("RNAME?");
  
  char response[40];
  char *remote_name = readResponseWithPrefix(response, sizeof(response), "+RNAME:");
  
  if (m_errorCode != HC05_OK)
    return false;
  
  if (!remote_name)
  {
    *buffer = 0;
    return readOperationResult() && false;
  }
  
  size_t copy_size = min(strlen(remote_name), buffer_size - 1);
  memcpy(buffer, remote_name, copy_size);
  buffer[copy_size] = 0;
  
  return readOperationResult();
}


bool Bluetooth_HC05::getRole(HC05_Role &role, unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("ROLE?");
  
  char response[20];
  const char *role_str = readResponseWithPrefix(response, sizeof(response), "+ROLE:");
  
  if (m_errorCode != HC05_OK)
    return false;
  
  if (!role_str)
    return readOperationResult() && false;
  
  role = static_cast<HC05_Role>(dec_u8::parse(role_str));//strtol(role_str, 0, 10));
  
  return readOperationResult();
}


bool Bluetooth_HC05::setRole(HC05_Role role, unsigned long timeout)
{
  startOperation(timeout);
  
  char role_str[10] = { role, 0 };
  sprintf(role_str, "%d", role);
  writeCommand("ROLE=", role_str);
  
  return readOperationResult();
}


bool Bluetooth_HC05::getDeviceClass(uint32_t &device_class, unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("CLASS?");
    device_class = 0;
  
  char response[40];
  const char *class_part = readResponseWithPrefix(response, sizeof(response), "+CLASS:");
  
  if (m_errorCode != HC05_OK)
    return false;
  
  if (!class_part)
    return readOperationResult() && false;
  
  device_class = hex_u32::parse(class_part);//strtoul(class_part, 0, 16);
  
  return readOperationResult();
}


bool Bluetooth_HC05::setDeviceClass(uint32_t device_class, unsigned long timeout)
{
  startOperation(timeout);
  
  char class_str[10];
  sprintf(class_str, "%lx", device_class);
  
  writeCommand("CLASS=", class_str);
  
  return readOperationResult();
}


bool Bluetooth_HC05::getInquiryAccessCode(uint32_t &iac, unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("IAC?");
  iac = 0;
  
  if (isOperationTimedOut())
    return false;
  
  char response[30];
  const char *iac_part = readResponseWithPrefix(response, sizeof(response), "+IAC:");
  
  if (m_errorCode != HC05_OK)
    return false;
  
  if (!iac_part)
    return readOperationResult() && false;
  
  iac = hex_u32::parse(iac_part);//strtoul(iac_part, 0, 16);
  
  return readOperationResult();
}


bool Bluetooth_HC05::setInquiryAccessCode(uint32_t iac, unsigned long timeout)
{
  startOperation(timeout);
  
  char iac_str[10];
  sprintf(iac_str, "%lx", iac);
  writeCommand("IAC=", iac_str);
  
  return readOperationResult();
}


bool Bluetooth_HC05::getInquiryMode(HC05_InquiryMode &inq_mode,
  int16_t &max_devices, uint8_t &max_duration, unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("INQM?");
  
  inq_mode = HC05_INQUIRY_STANDARD;
  max_devices = 0;
  max_duration = 0;
  
  char response[30];
  const char *mode_part = readResponseWithPrefix(response, sizeof(response), "+INQM:");
  
  if (m_errorCode != HC05_OK)
    return false;
    
  if (!mode_part)
    return readOperationResult() && false;
  
  inq_mode = static_cast<HC05_InquiryMode>(dec_u8::parse(mode_part));
  
  if (*mode_part != ',')
    return readOperationResult() && false;
  
  ++mode_part;
  max_devices = dec_i16::parse(mode_part);
  
  if (*mode_part != ',')
    return readOperationResult() && false;
  
  ++mode_part;
  max_duration = dec_u8::parse(mode_part);
  
  return readOperationResult();
}


bool Bluetooth_HC05::setInquiryMode(HC05_InquiryMode inq_mode,
  int16_t max_devices, uint8_t max_duration, unsigned long timeout)
{
  startOperation(timeout);
  
  char mode[20];
  /* Yeah, max_devices is signed 16-bit integer, but the module accepts
   * an unsigned 16-bit integer while actually interpreting it as signed.
   *                          <-^->
   * Tricky chinese engineers (-_-)
   *                            "
   */
  sprintf(mode, "%d,%u,%u", inq_mode, (uint16_t)max_devices, max_duration);
  
  writeCommand("INQM=", mode);
  
  return readOperationResult();
}


bool Bluetooth_HC05::getPassword(char *buffer, unsigned long timeout)
{
  if (!buffer)
    return false;
    
  startOperation(timeout);
  writeCommand("PSWD?");
  
  char response[30];
  const char *password = readResponseWithPrefix(response, sizeof(response), "+PSWD:");
  
  if (m_errorCode != HC05_OK)
  {
    *buffer = 0;
    return false;
  }
  
  if (!password)
  {
    *buffer = 0;
    return readOperationResult() && false;
  }
  
  strcpy(buffer, password);
  
  return readOperationResult();
  
}


bool Bluetooth_HC05::setPassword(const char *password, unsigned long timeout)
{
  if (!password)
    return false;
    
  startOperation(timeout);
  writeCommand("PSWD=", password);
  
  return readOperationResult();
}


bool Bluetooth_HC05::getSerialMode(uint32_t &speed, uint8_t &stop_bits,
  HC05_Parity &parity, unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("UART?");
  
  char response[30];
  const char *mode_str = readResponseWithPrefix(response, sizeof(response), "+UART:");
  
  if (m_errorCode != HC05_OK)
    return false;
    
  if (!mode_str)
    return readOperationResult() && false;
    
  speed = dec_u32::parse(mode_str);
  
  if (*mode_str != ',')
  {
    m_errorCode = HC05_FAIL;
    return readOperationResult() && false;
  }
  
  ++mode_str;
  stop_bits = dec_u8::parse(mode_str) + 1;
  
  if (*mode_str != ',')
  {
    m_errorCode = HC05_FAIL;
    return readOperationResult() && false;
  }
  
  ++mode_str;
  parity = static_cast<HC05_Parity>(dec_u8::parse(mode_str));
  
  return readOperationResult();
}


bool Bluetooth_HC05::setSerialMode(uint32_t speed, uint8_t stop_bits,
  HC05_Parity parity, unsigned long timeout)
{
  startOperation(timeout);
  
  stop_bits -= 1; // 0: 1 stop bit, 1: 2 stop bits, any other are not allowed
  
  char mode_str[20];
  sprintf(mode_str, "%lu,%u,%u", speed, stop_bits, parity);
  writeCommand("UART=", mode_str);
  
  return readOperationResult();
}


bool Bluetooth_HC05::getConnectionMode(
  HC05_Connection &connection_mode, unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("CMODE?");
  
  char response[20];
  const char *mode_part = readResponseWithPrefix(response, sizeof(response), "+CMODE:");
  
  if (m_errorCode != HC05_OK)
    return false;
    
  if (!mode_part)
    return readOperationResult() && false;
  
  connection_mode = static_cast<HC05_Connection>(dec_u8::parse(mode_part));
  
  return readOperationResult();
}


bool Bluetooth_HC05::setConnectionMode(
  HC05_Connection connection_mode, unsigned long timeout)
{
  startOperation(timeout);
  
  char mode_str[20];
  sprintf(mode_str, "%u", connection_mode);
  writeCommand("CMODE=", mode_str);
  
  return readOperationResult();
}


bool Bluetooth_HC05::bind(const BluetoothAddress &address, unsigned long timeout)
{
  return writeAddressWithCommand(address, "BIND", timeout);
}


bool Bluetooth_HC05::getAddressBound(BluetoothAddress &address, unsigned long timeout)
{
  return readAddressWithCommand(address, "BIND", timeout);
}


bool Bluetooth_HC05::getLeds(bool &led_status,
  bool &led_connection, unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("POLAR?");
  
  led_status = 0;
  led_connection = 0;
  
  char response[30];
  const char *status_part = readResponseWithPrefix(response, sizeof(response), "+POLAR:");
  
  if (m_errorCode != HC05_OK)
    return false;
    
  if (!status_part)
    return readOperationResult() && false;
  
  led_status = dec_u8::parse(status_part);
  
  if (*status_part != ',')
    return readOperationResult() && false;
  
  ++status_part;
  led_connection = dec_u8::parse(status_part);
  
  return readOperationResult();
}


bool Bluetooth_HC05::setLeds(bool led_status,
  bool led_connection, unsigned long timeout)
{
  startOperation(timeout);
  
  char leds_str[10];
  sprintf(leds_str, "%d,%d", (led_status ? 1 : 0), (led_connection ? 1 : 0));
  writeCommand("POLAR=", leds_str);
  
  return readOperationResult();
}


bool Bluetooth_HC05::setPortState(uint8_t port_num, uint8_t port_state, unsigned long timeout)
{
  startOperation(timeout);
  
  char state_str[10];
  sprintf(state_str, "%u,%u", port_num, port_state);
  writeCommand("PIO=", state_str);
  
  return readOperationResult();
}


bool Bluetooth_HC05::getMultiplePorts(uint16_t &port_states, unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("MPIO?");
  
  char response[20];
  const char *states_part = readResponseWithPrefix(response, sizeof(response), "+MPIO:");
  
  if (m_errorCode != HC05_OK)
    return false;
  
  if (!states_part)
    return readOperationResult() && false;
  
  port_states = hex_u16::parse(states_part);
  
  return readOperationResult();
}


bool Bluetooth_HC05::setMultiplePorts(uint16_t port_states, unsigned long timeout)
{
  startOperation(timeout);
  
  char states_str[10];
  sprintf(states_str, "%x", port_states);
  writeCommand("MPIO=", states_str);
  
  return readOperationResult();
}


bool Bluetooth_HC05::getInquiryAndPagingParams(
  uint16_t &inquiry_interval, uint16_t &inquiry_duration,
  uint16_t &paging_interval, uint16_t &paging_duration, unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("IPSCAN?");
  
  char response[40];
  const char *params_part = readResponseWithPrefix(response, sizeof(response), "+IPSCAN:");
  
  if (m_errorCode != HC05_OK)
    return false;
  
  if (!params_part)
    return readOperationResult() && false;
  
  inquiry_interval = dec_u16::parse(params_part);
  
  if (*params_part != ',')
    return readOperationResult() && false;
    
  ++params_part;
  inquiry_duration = dec_u16::parse(params_part);
  
  if (*params_part != ',')
    return readOperationResult() && false;
    
  ++params_part;
  paging_interval = dec_u16::parse(params_part);
  
  if (*params_part != ',')
    return readOperationResult() && false;
    
  ++params_part;
  paging_duration = dec_u16::parse(params_part);
  
  return readOperationResult();
}


bool Bluetooth_HC05::setInquiryAndPagingParams(
  uint16_t inquiry_interval, uint16_t inquiry_duration,
  uint16_t paging_interval, uint16_t paging_duration, unsigned long timeout)
{
  startOperation(timeout);
  
  char params_str[40];
  sprintf(params_str, "%u,%u,%u,%u",
    inquiry_interval, inquiry_duration, paging_interval, paging_duration);
  writeCommand("IPSCAN=", params_str);
  
  return readOperationResult();
}


bool Bluetooth_HC05::getSniffParams(uint16_t &max_time, uint16_t &min_time,
  uint16_t &retry_interval, uint16_t &sniff_timeout, unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("SNIFF?");
  
  char response[40];
  const char *params_part = readResponseWithPrefix(response, sizeof(response), "+SNIFF:");
  
  if (m_errorCode != HC05_OK)
    return false;
  
  if (!params_part)
    return readOperationResult() && false;
  
  max_time = dec_u16::parse(params_part);
  
  if (*params_part != ',')
    return readOperationResult() && false;
    
  ++params_part;
  min_time = dec_u16::parse(params_part);
  
  if (*params_part != ',')
    return readOperationResult() && false;
    
  ++params_part;
  retry_interval = dec_u16::parse(params_part);
  
  if (*params_part != ',')
    return readOperationResult() && false;
    
  ++params_part;
  sniff_timeout = dec_u16::parse(params_part);
  
  return readOperationResult();
}


bool Bluetooth_HC05::setSniffParams(uint16_t max_time, uint16_t min_time,
  uint16_t retry_interval, uint16_t sniff_timeout, unsigned long timeout)
{
  startOperation(timeout);
  
  char params_str[40];
  sprintf(params_str, "%u,%u,%u,%u", max_time, min_time, retry_interval, sniff_timeout);
  writeCommand("SNIFF=", params_str);
  
  return readOperationResult();
}


bool Bluetooth_HC05::enterSniffMode(unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("ENSNIFF");
  return readOperationResult();
}


bool Bluetooth_HC05::exitSniffMode(unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("EXSNIFF");
  return readOperationResult();
}


bool Bluetooth_HC05::getSecurityAndEncryption(HC05_Security &security,
  HC05_Encryption &encryption, unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("SENM?");
  
  char response[20];
  const char *params_part = readResponseWithPrefix(response, sizeof(response), "+SENM:");
  
  if (m_errorCode != HC05_OK)
    return false;
  
  if (!params_part)
    return readOperationResult() && false;
  
  security = static_cast<HC05_Security>(dec_u8::parse(params_part));
  
  if (*params_part != ',')
    return readOperationResult() && false;
    
  ++params_part;
  encryption = static_cast<HC05_Encryption>(dec_u8::parse(params_part));
  
  return readOperationResult();
}


bool Bluetooth_HC05::setSecurityAndEncryption(HC05_Security security,
  HC05_Encryption encryption, unsigned long timeout)
{
  startOperation(timeout);
  
  char params_str[10];
  sprintf(params_str, "%u,%u", security, encryption);
  writeCommand("SENM=", params_str);
  
  return readOperationResult();
}


bool Bluetooth_HC05::deleteDeviceFromList(
    const BluetoothAddress &address, unsigned long timeout)
{
  return writeAddressWithCommand(address, "RMSAD", timeout);
}


bool Bluetooth_HC05::deleteAllDevicesFromList(unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("RMAAD");
  return readOperationResult();
}


bool Bluetooth_HC05::findDeviceInList(
  const BluetoothAddress &address, unsigned long timeout)
{
  //return writeAddressWithCommand(address, "FSAD", timeout);
  startOperation(timeout);
  
  //char command[30];
  //sprintf(command, "%s=", command_name);
  char address_str[BLUETOOTH_ADDRESS_MAXLEN + 1];
  printBluetoothAddress(address_str, address, ',');
  
  writeCommand("FSAD=", address_str);
  
  return readOperationResult();
}


bool Bluetooth_HC05::deviceCountInList(uint8_t &device_count, unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("ADCN?");
  
  char response[20];
  const char *count_part = readResponseWithPrefix(response, sizeof(response), "+ADCN:");
  
  if (m_errorCode != HC05_OK)
    return false;
  
  if (!count_part)
    return readOperationResult() && false;
  
  device_count = dec_u8::parse(count_part);
  
  return readOperationResult();
}


bool Bluetooth_HC05::getLastAuthenticatedDevice(
  BluetoothAddress &address, unsigned long timeout)
{
  return readAddressWithCommand(address, "MRAD", timeout);
}


bool Bluetooth_HC05::readAddressWithCommand(BluetoothAddress &address,
  const char *command_name, unsigned long timeout)
{
  startOperation(timeout);
  memset(address, 0, sizeof(BluetoothAddress));
  
  if (!command_name)
    return false;
  
  writeCommand(command_name, "?");
  
  /* Response should look like "+<command_name>:<NAP>:<UAP>:<LAP>",
   * where actual address will look like "1234:56:abcdef".
   */
  char response[BLUETOOTH_ADDRESS_MAXLEN + 20];
  char response_pattern[20];
  sprintf(response_pattern, "+%s:", command_name);
  char *addr_part = readResponseWithPrefix(response, sizeof(response), response_pattern);
  
  if (m_errorCode != HC05_OK)
    return false;

  if (!addr_part)
    return readOperationResult() && false;
  
  if (!parseBluetoothAddress(address, addr_part))
    return readOperationResult() && false;
  
  return readOperationResult();
}


bool Bluetooth_HC05::writeAddressWithCommand(const BluetoothAddress &address,
  const char *command_name, unsigned long timeout)
{
  startOperation(timeout);
  
  char command[20];
  sprintf(command, "%s=", command_name);
  
  char address_str[BLUETOOTH_ADDRESS_MAXLEN + 1];
  printBluetoothAddress(address_str, address, ',');
  
  writeCommand(command, address_str);
  
  return readOperationResult();
}


bool Bluetooth_HC05::readOperationResult()
{
  char response[20];
  readLine(response, sizeof(response));
  
  return strcmp(response, "OK") == 0;
}


void Bluetooth_HC05::writeCommand(const char *command, const char *arg)
{
  m_uart->print("AT");
  
  if (command && command[0] != 0)
  {
    m_uart->write('+');
    m_uart->print(command);
  }
  
  if (arg && arg[0] != 0)
    m_uart->print(arg);
  
  m_uart->print("\r\n");
}


size_t Bluetooth_HC05::readLine(char *buffer, size_t buffer_size)
{
  if (!buffer || buffer_size <= 1)
    return 0;

  char *p = buffer;
  *p = 0;

  while ((p - buffer < buffer_size - 1) && (p[-1] != '\n' || p[-2] != '\r'))
  {
    while (!m_uart->available())
    {
      if (isOperationTimedOut())
        goto EXIT_LOOP;
    }
    
    *p++ = m_uart->read();
    continue;
    
  EXIT_LOOP:
    break;
  }

  if (p[-1] == '\n' && p[-2] == '\r')
    p -= 2;
  
  *p = 0;
  
  if (const char *error_code_str = skipPrefix(buffer, buffer_size, "ERROR:("))
    m_errorCode = static_cast<HC05_Result>(hex_u8::parse(error_code_str));//strtoul(error_code_str, 0, 16));
  else if (strcmp(buffer, "FAIL") == 0)
    m_errorCode = HC05_FAIL;

  size_t num_bytes = p - buffer;
  return num_bytes;
}


bool Bluetooth_HC05::parseBluetoothAddress(BluetoothAddress &address, const char *address_str)
{
  /* Address should look like "+ADDR:<NAP>:<UAP>:<LAP>",
   * where actual address will look like "1234:56:abcdef".
   */
  if (!address || !address_str)
    return false;
  
  const char *digits_ptr = const_cast<char*>(address_str);
  uint8_t NAP[2];
  *((uint16_t*)NAP) = hex_u16::parse(digits_ptr);//strtol(digits_ptr, &digits_ptr, 16);
  
  if (*digits_ptr != ':')
    return false;
  
  ++digits_ptr;
  uint8_t UAP = hex_u8::parse(digits_ptr);//strtol(digits_ptr, &digits_ptr, 16);
  
  if (*digits_ptr != ':')
    return false;
  
  ++digits_ptr;
  uint8_t LAP[4];
  *((uint32_t*)LAP) = hex_u32::parse(digits_ptr);//strtol(digits_ptr, &digits_ptr, 16);
  
  address[0] = NAP[1];
  address[1] = NAP[0];
  address[2] = UAP;
  address[3] = LAP[2];
  address[4] = LAP[1];
  address[5] = LAP[0];
  
  return true;
}


void Bluetooth_HC05::printBluetoothAddress(char *address_str,
  const BluetoothAddress &address, char delimiter)
{
  if (address && address_str)
  {
    uint8_t NAP[2];
    NAP[0] = address[1];
    NAP[1] = address[0];
    
    uint8_t UAP = address[2];
    
    uint8_t LAP[4];
    LAP[0] = address[5];
    LAP[1] = address[4];
    LAP[2] = address[3];
    LAP[3] = 0;
    
    sprintf(address_str, "%x%c%x%c%lx",
      *reinterpret_cast<const uint16_t*>(NAP),
      delimiter,
      UAP,
      delimiter,
      *reinterpret_cast<const uint32_t*>(LAP));
  }
}


char *Bluetooth_HC05::readResponseWithPrefix(char *buffer, size_t buffer_size, char *prefix)
{
  if (!buffer || buffer_size <= 1)
    return 0;
  
  size_t response_length = readLine(buffer, buffer_size);
  char *postfix = skipPrefix(buffer, response_length, prefix);
  
  if (!postfix)
    *buffer = 0;

  return postfix;
}


char *Bluetooth_HC05::skipPrefix(char *str, size_t str_length, char *prefix)
{
  if (!str || str_length == 0 || !prefix)
    return 0;
  
  size_t prefix_length = strlen(prefix);
  
  if (str_length >= prefix_length && strncmp(str, prefix, prefix_length) == 0)
    return str + prefix_length;
  
  return 0;
}


void Bluetooth_HC05::startOperation(unsigned long timeout)
{
  m_ticksAtStart = millis();
  m_timeout = timeout;
  m_errorCode = HC05_OK;
}


bool Bluetooth_HC05::isOperationTimedOut() const
{
  return operationDuration() >= m_timeout;
}


unsigned long Bluetooth_HC05::operationDuration() const
{
  unsigned long current_ticks = millis(),
                elapsed_ticks;
      
  if (current_ticks >= m_ticksAtStart)
    elapsed_ticks = current_ticks - m_ticksAtStart;
  else
    elapsed_ticks = (ULONG_MAX - m_ticksAtStart) + current_ticks;
  
  return elapsed_ticks;
}


void Bluetooth_HC05::write(uint8_t data)
{
  m_uart->write(data);
}
