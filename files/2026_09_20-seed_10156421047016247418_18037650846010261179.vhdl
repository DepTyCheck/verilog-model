-- Seed: 10156421047016247418,18037650846010261179

library ieee;
use ieee.std_logic_1164.all;

entity xkgets is
  port (qmczxpk : inout time; ujec : linkage std_logic; sgjvxwqy : buffer integer; ykywdi : inout std_logic_vector(1 downto 2));
end xkgets;

architecture dtlyvelau of xkgets is
  
begin
  -- Multi-driven assignments
  ykywdi <= (others => '0');
  ykywdi <= (others => '0');
  ykywdi <= ykywdi;
  ykywdi <= (others => '0');
end dtlyvelau;

entity ovkmd is
  port (obngmuhytv : in bit_vector(3 to 0); dhetezapsy : buffer time; sr : linkage real; klfyg : inout real);
end ovkmd;

library ieee;
use ieee.std_logic_1164.all;

architecture clecs of ovkmd is
  signal ynpbbdfx : std_logic_vector(1 downto 2);
  signal aksygbu : integer;
  signal snthc : std_logic_vector(1 downto 2);
  signal nxdsgz : integer;
  signal nvaq : std_logic;
  signal s : time;
begin
  jed : entity work.xkgets
    port map (qmczxpk => s, ujec => nvaq, sgjvxwqy => nxdsgz, ykywdi => snthc);
  jcsua : entity work.xkgets
    port map (qmczxpk => dhetezapsy, ujec => nvaq, sgjvxwqy => aksygbu, ykywdi => ynpbbdfx);
  
  -- Single-driven assignments
  klfyg <= 8#6.7067#;
  
  -- Multi-driven assignments
  ynpbbdfx <= snthc;
  nvaq <= 'W';
  nvaq <= 'L';
end clecs;



-- Seed after: 7731358676848210253,18037650846010261179
