-- Seed: 6405791838223950842,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity hirtomafur is
  port (ywv : inout std_logic_vector(4 downto 0); liahx : out integer; mdqhewldnw : buffer std_logic_vector(1 downto 1));
end hirtomafur;

architecture eqrqj of hirtomafur is
  
begin
  -- Single-driven assignments
  liahx <= 2#0_0#;
  
  -- Multi-driven assignments
  ywv <= ('X', 'H', '1', '-', 'L');
end eqrqj;

library ieee;
use ieee.std_logic_1164.all;

entity qbdludhdwd is
  port (fjwqojkbdl : out real; trcpgkvw : inout std_logic; himjqut : out bit);
end qbdludhdwd;

library ieee;
use ieee.std_logic_1164.all;

architecture anzvkbhrj of qbdludhdwd is
  signal d : std_logic_vector(1 downto 1);
  signal jsmrhpr : integer;
  signal izwa : std_logic_vector(4 downto 0);
  signal kg : std_logic_vector(1 downto 1);
  signal udr : integer;
  signal gnzioskkj : std_logic_vector(4 downto 0);
  signal xqwpbyvyks : std_logic_vector(1 downto 1);
  signal akqovniqwo : integer;
  signal bmuw : std_logic_vector(1 downto 1);
  signal iebearik : integer;
  signal pxnmbcxi : std_logic_vector(4 downto 0);
begin
  luzghvvt : entity work.hirtomafur
    port map (ywv => pxnmbcxi, liahx => iebearik, mdqhewldnw => bmuw);
  i : entity work.hirtomafur
    port map (ywv => pxnmbcxi, liahx => akqovniqwo, mdqhewldnw => xqwpbyvyks);
  iwm : entity work.hirtomafur
    port map (ywv => gnzioskkj, liahx => udr, mdqhewldnw => kg);
  sjxbcdyfkt : entity work.hirtomafur
    port map (ywv => izwa, liahx => jsmrhpr, mdqhewldnw => d);
  
  -- Single-driven assignments
  fjwqojkbdl <= 1_0_0.3;
  himjqut <= '1';
  
  -- Multi-driven assignments
  izwa <= "Z1-11";
  xqwpbyvyks <= (others => '1');
  izwa <= "ULH-W";
end anzvkbhrj;

library ieee;
use ieee.std_logic_1164.all;

entity fbzw is
  port (p : in time; eosx : buffer std_logic_vector(2 to 2); ofjmoqrbn : out std_logic; k : in std_logic);
end fbzw;

architecture rcb of fbzw is
  
begin
  -- Multi-driven assignments
  ofjmoqrbn <= 'L';
end rcb;

entity ippdr is
  port (i : in integer; ztaghuguo : out integer; asklxm : out bit; ls : out time);
end ippdr;

library ieee;
use ieee.std_logic_1164.all;

architecture c of ippdr is
  signal kazmb : integer;
  signal qtnvzhoib : std_logic_vector(4 downto 0);
  signal xckwfmlz : std_logic_vector(1 downto 1);
  signal jca : std_logic_vector(4 downto 0);
  signal czdhwjrwa : std_logic_vector(2 to 2);
  signal gll : std_logic;
  signal etusq : std_logic_vector(2 to 2);
  signal qvusdbolyn : time;
begin
  msvhp : entity work.fbzw
    port map (p => qvusdbolyn, eosx => etusq, ofjmoqrbn => gll, k => gll);
  qdpv : entity work.fbzw
    port map (p => ls, eosx => czdhwjrwa, ofjmoqrbn => gll, k => gll);
  kihxdmb : entity work.hirtomafur
    port map (ywv => jca, liahx => ztaghuguo, mdqhewldnw => xckwfmlz);
  ogsqunowj : entity work.hirtomafur
    port map (ywv => qtnvzhoib, liahx => kazmb, mdqhewldnw => xckwfmlz);
  
  -- Single-driven assignments
  ls <= 4 sec;
end c;



-- Seed after: 9219512287591212748,1834764876137802293
