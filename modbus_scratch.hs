
def read_rtu_response(io)
        # Read the slave_id and function code
  msg = nil
  while msg.nil?
          msg = io.read(2)
  end

  function_code = msg.getbyte(1)
  case function_code
    when 1,2,3,4 then
      # read the third byte to find out how much more
      # we need to read + CRC
      msg += io.read(1)
      msg += io.read(msg.getbyte(2)+2)
    when 5,6,15,16 then
      # We just read in an additional 6 bytes
      msg += io.read(6)
    when 22 then
      msg += io.read(8)
    when 0x80..0xff then
      msg += io.read(3)
    else
      raise ModBus::Errors::IllegalFunction, "Illegal function: #{function_code}"
  end
end

def read_rtu_request(io)
    # Read the slave_id and function code
    msg = io.read(2)

    # If msg is nil, then our client never sent us anything and it's time to disconnect
    return if msg.nil?

    function_code = msg.getbyte(1)
    if [1, 2, 3, 4, 5, 6].include?(function_code)
            # read 6 more bytes and return the message total message
            msg += io.read(6)
    elsif [15, 16].include?(function_code)
            # Read in first register, register count, and data bytes
            msg += io.read(5)
            # Read in however much data we need to + 2 CRC bytes
            msg += io.read(msg.getbyte(6) + 2)
    else
            raise ModBus::Errors::IllegalFunction, "Illegal function: #{function_code}"
    end

    log "Server RX (#{msg.size} bytes): #{logging_bytes(msg)}"

    msg
end
