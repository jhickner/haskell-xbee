#!/usr/bin/env ruby

require 'rmodbus'

begin
  ModBus::RTUViaTCPClient.new('127.0.0.1', 8000) do |cl|
    cl.with_slave(254) do |slave|
      slave.read_retries = 0
      # Read a single holding register at address 16
      slave.holding_registers[16]

      # Write a single holding register at address 16
      #slave.holding_registers[16] = 123

      # Read holding registers 16 through 20
      #slave.holding_registers[16..20]

      # Write holding registers 16 through 20 with some values
      #slave.holding_registers[16..20] = [1, 2, 3, 4, 5]
    end
  end
rescue 
  print "Timed out!\n"
end
