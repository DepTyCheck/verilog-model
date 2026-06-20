-- Seed: 16855881220507284883,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity ddsytg is
  port (nfndozpb : in bit; otpwszfaix : linkage boolean_vector(0 to 1); up : buffer std_logic);
end ddsytg;

architecture bri of ddsytg is
  
begin
  -- Multi-driven assignments
  up <= 'Z';
  up <= 'Z';
  up <= 'X';
  up <= 'Z';
end bri;

library ieee;
use ieee.std_logic_1164.all;

entity lczku is
  port (uywqeyfa : inout time; f : inout time_vector(4 to 4); svrkunz : buffer std_logic_vector(3 downto 4); ohxcmxzorm : linkage time);
end lczku;

library ieee;
use ieee.std_logic_1164.all;

architecture kahjd of lczku is
  signal dofffnhy : std_logic;
  signal akfqoqrkus : boolean_vector(0 to 1);
  signal soolilghg : bit;
  signal vo : std_logic;
  signal yfblmzi : boolean_vector(0 to 1);
  signal rmqirjrz : std_logic;
  signal yeyi : boolean_vector(0 to 1);
  signal ye : bit;
begin
  eh : entity work.ddsytg
    port map (nfndozpb => ye, otpwszfaix => yeyi, up => rmqirjrz);
  niq : entity work.ddsytg
    port map (nfndozpb => ye, otpwszfaix => yfblmzi, up => vo);
  izm : entity work.ddsytg
    port map (nfndozpb => soolilghg, otpwszfaix => akfqoqrkus, up => dofffnhy);
  
  -- Single-driven assignments
  f <= (others => 2#11111# fs);
  soolilghg <= '1';
  ye <= '0';
  uywqeyfa <= 4.0 ms;
  
  -- Multi-driven assignments
  rmqirjrz <= '-';
  dofffnhy <= 'Z';
  svrkunz <= (others => '0');
end kahjd;



-- Seed after: 2934952060436883464,17924494779688682807
