-- Seed: 2646118046838516803,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity lyvud is
  port (hyflo : in real; j : linkage real_vector(3 to 0); rcvmfhz : out time; wybz : buffer std_logic_vector(0 to 2));
end lyvud;

architecture thg of lyvud is
  
begin
  -- Single-driven assignments
  rcvmfhz <= 8#47# us;
  
  -- Multi-driven assignments
  wybz <= ('0', 'U', 'U');
  wybz <= "01L";
  wybz <= ('X', '-', 'Z');
  wybz <= "1-W";
end thg;

library ieee;
use ieee.std_logic_1164.all;

entity fg is
  port (acvxo : inout character; ohf : buffer real; az : buffer std_logic_vector(2 to 1));
end fg;

library ieee;
use ieee.std_logic_1164.all;

architecture tx of fg is
  signal nhlnoh : time;
  signal tb : real_vector(3 to 0);
  signal qqi : real;
  signal ousvsfs : std_logic_vector(0 to 2);
  signal ticgm : time;
  signal yqy : real_vector(3 to 0);
  signal ujnliqkviq : std_logic_vector(0 to 2);
  signal d : time;
  signal yrsxu : real_vector(3 to 0);
begin
  jmcarvkc : entity work.lyvud
    port map (hyflo => ohf, j => yrsxu, rcvmfhz => d, wybz => ujnliqkviq);
  m : entity work.lyvud
    port map (hyflo => ohf, j => yqy, rcvmfhz => ticgm, wybz => ousvsfs);
  scuvkvawp : entity work.lyvud
    port map (hyflo => qqi, j => tb, rcvmfhz => nhlnoh, wybz => ujnliqkviq);
  
  -- Multi-driven assignments
  az <= "";
end tx;

library ieee;
use ieee.std_logic_1164.all;

entity vabuawfpud is
  port (xr : buffer std_logic; ybxnbhq : buffer time_vector(3 downto 2));
end vabuawfpud;

architecture wgzmdgnjea of vabuawfpud is
  
begin
  -- Single-driven assignments
  ybxnbhq <= (2#1_0_1_1# ns, 1 hr);
  
  -- Multi-driven assignments
  xr <= 'H';
  xr <= '1';
  xr <= 'Z';
  xr <= '0';
end wgzmdgnjea;



-- Seed after: 11330157923651731914,3108530264173481209
