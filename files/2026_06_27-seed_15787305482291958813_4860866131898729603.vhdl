-- Seed: 15787305482291958813,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (mfsum : out real; ieymhkq : in real; qwf : inout time; vofh : out std_logic_vector(0 downto 4));
end d;

architecture r of d is
  
begin
  -- Single-driven assignments
  qwf <= 1 hr;
  
  -- Multi-driven assignments
  vofh <= "";
  vofh <= "";
  vofh <= (others => '0');
end r;

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (nvbpqfoeu : inout std_logic; aaaz : linkage time_vector(1 to 1); cqlnkeipu : inout std_logic_vector(3 to 1));
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture zogmxrk of h is
  signal fy : time;
  signal kdjb : real;
  signal swzanbrw : time;
  signal nl : std_logic_vector(0 downto 4);
  signal mjxnrdn : time;
  signal partspj : real;
  signal zzjew : std_logic_vector(0 downto 4);
  signal cuaeibyxk : time;
  signal m : real;
  signal fqpul : real;
begin
  vhdm : entity work.d
    port map (mfsum => fqpul, ieymhkq => m, qwf => cuaeibyxk, vofh => zzjew);
  pztwp : entity work.d
    port map (mfsum => partspj, ieymhkq => fqpul, qwf => mjxnrdn, vofh => nl);
  itire : entity work.d
    port map (mfsum => m, ieymhkq => fqpul, qwf => swzanbrw, vofh => cqlnkeipu);
  idfzlzdtnd : entity work.d
    port map (mfsum => kdjb, ieymhkq => m, qwf => fy, vofh => nl);
  
  -- Multi-driven assignments
  nl <= (others => '0');
  cqlnkeipu <= (others => '0');
  cqlnkeipu <= (others => '0');
  zzjew <= (others => '0');
end zogmxrk;



-- Seed after: 11389231072499451926,4860866131898729603
