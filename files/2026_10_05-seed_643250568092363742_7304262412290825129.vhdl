-- Seed: 643250568092363742,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity pychd is
  port (fdazwjjd : inout character; dujngstwol : inout std_logic; ronedkvc : inout std_logic);
end pychd;

architecture mbunnnw of pychd is
  
begin
  -- Single-driven assignments
  fdazwjjd <= 'o';
  
  -- Multi-driven assignments
  ronedkvc <= '0';
  dujngstwol <= ronedkvc;
  ronedkvc <= ronedkvc;
  ronedkvc <= ronedkvc;
end mbunnnw;

library ieee;
use ieee.std_logic_1164.all;

entity ki is
  port (n : buffer real_vector(2 to 0); xsnnavrs : linkage real; wemitbdy : linkage std_logic_vector(3 to 4); cj : buffer bit_vector(1 downto 0));
end ki;

library ieee;
use ieee.std_logic_1164.all;

architecture gaxjzrk of ki is
  signal xvndcul : character;
  signal bmiesibq : std_logic;
  signal xhx : character;
  signal cpxjdemf : std_logic;
  signal dikc : character;
  signal qakd : std_logic;
  signal xaxvgqt : character;
begin
  rlgvzwdjn : entity work.pychd
    port map (fdazwjjd => xaxvgqt, dujngstwol => qakd, ronedkvc => qakd);
  ye : entity work.pychd
    port map (fdazwjjd => dikc, dujngstwol => cpxjdemf, ronedkvc => qakd);
  ofpwmk : entity work.pychd
    port map (fdazwjjd => xhx, dujngstwol => qakd, ronedkvc => bmiesibq);
  loazelhjp : entity work.pychd
    port map (fdazwjjd => xvndcul, dujngstwol => qakd, ronedkvc => qakd);
  
  -- Single-driven assignments
  cj <= ('1', '1');
  
  -- Multi-driven assignments
  qakd <= 'X';
end gaxjzrk;



-- Seed after: 8240754405351361331,7304262412290825129
