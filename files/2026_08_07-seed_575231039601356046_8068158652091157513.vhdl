-- Seed: 575231039601356046,8068158652091157513

library ieee;
use ieee.std_logic_1164.all;

entity pm is
  port (phrfm : linkage std_logic_vector(2 downto 2); lmnta : buffer std_logic_vector(0 to 3); xbzoihw : linkage std_logic);
end pm;

architecture qlj of pm is
  
begin
  -- Multi-driven assignments
  lmnta <= ('0', 'W', '1', '-');
  lmnta <= ('U', 'W', 'Z', '0');
end qlj;

entity xzd is
  port (yjqfyt : in character; bpufqb : out integer_vector(0 to 0); tscrkuf : in time);
end xzd;

library ieee;
use ieee.std_logic_1164.all;

architecture k of xzd is
  signal s : std_logic;
  signal wjr : std_logic_vector(0 to 3);
  signal zok : std_logic_vector(0 to 3);
  signal eizvndbe : std_logic_vector(0 to 3);
  signal gaqiyggp : std_logic_vector(2 downto 2);
  signal yrbzptck : std_logic;
  signal dwmixqennu : std_logic_vector(0 to 3);
  signal hzve : std_logic_vector(2 downto 2);
begin
  mbchkt : entity work.pm
    port map (phrfm => hzve, lmnta => dwmixqennu, xbzoihw => yrbzptck);
  gqifbxvl : entity work.pm
    port map (phrfm => gaqiyggp, lmnta => eizvndbe, xbzoihw => yrbzptck);
  iv : entity work.pm
    port map (phrfm => hzve, lmnta => zok, xbzoihw => yrbzptck);
  ncznbpge : entity work.pm
    port map (phrfm => hzve, lmnta => wjr, xbzoihw => s);
  
  -- Single-driven assignments
  bpufqb <= (others => 16#4#);
  
  -- Multi-driven assignments
  s <= yrbzptck;
end k;



-- Seed after: 13118514972471483994,8068158652091157513
