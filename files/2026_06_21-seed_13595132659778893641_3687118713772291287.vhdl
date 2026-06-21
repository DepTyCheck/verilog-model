-- Seed: 13595132659778893641,3687118713772291287

entity idrbxyrxsi is
  port (cniwyvycu : buffer bit; jmk : inout integer);
end idrbxyrxsi;

architecture gdkqh of idrbxyrxsi is
  
begin
  -- Single-driven assignments
  cniwyvycu <= '0';
  jmk <= 8#1#;
end gdkqh;

library ieee;
use ieee.std_logic_1164.all;

entity dhcmkc is
  port (yfpquufng : in integer; jmspf : inout real; kg : in std_logic_vector(1 to 3); daynt : linkage time);
end dhcmkc;

architecture amlxmk of dhcmkc is
  signal rxcw : integer;
  signal wccq : bit;
  signal ltfryptoa : integer;
  signal nvsgegl : bit;
  signal n : integer;
  signal gvajg : bit;
begin
  kna : entity work.idrbxyrxsi
    port map (cniwyvycu => gvajg, jmk => n);
  ww : entity work.idrbxyrxsi
    port map (cniwyvycu => nvsgegl, jmk => ltfryptoa);
  l : entity work.idrbxyrxsi
    port map (cniwyvycu => wccq, jmk => rxcw);
  
  -- Single-driven assignments
  jmspf <= 2#0_1_0_1.10#;
end amlxmk;

library ieee;
use ieee.std_logic_1164.all;

entity aq is
  port (qjqmvn : out real; fsxjmzd : inout std_logic_vector(4 downto 0); wsehbire : inout real; lqocl : in std_logic);
end aq;

library ieee;
use ieee.std_logic_1164.all;

architecture b of aq is
  signal apoac : time;
  signal amin : std_logic_vector(1 to 3);
  signal kxan : real;
  signal e : integer;
  signal tleg : bit;
begin
  aho : entity work.idrbxyrxsi
    port map (cniwyvycu => tleg, jmk => e);
  ucyfjod : entity work.dhcmkc
    port map (yfpquufng => e, jmspf => kxan, kg => amin, daynt => apoac);
  
  -- Single-driven assignments
  wsehbire <= 3_0_0.4122;
  qjqmvn <= 2#1_0_1.01010#;
  
  -- Multi-driven assignments
  fsxjmzd <= ('1', 'X', 'L', 'U', 'X');
  fsxjmzd <= ('L', 'H', 'H', 'H', 'X');
end b;



-- Seed after: 17022146703104236450,3687118713772291287
