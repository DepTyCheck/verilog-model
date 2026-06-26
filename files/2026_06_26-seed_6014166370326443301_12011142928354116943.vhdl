-- Seed: 6014166370326443301,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (ver : linkage bit; ygid : buffer std_logic_vector(2 downto 0); lhuwf : inout integer);
end w;

architecture ndhownpdwd of w is
  
begin
  
end ndhownpdwd;

library ieee;
use ieee.std_logic_1164.all;

entity huctbrcusr is
  port (pxkuq : out integer; gj : out std_logic_vector(4 to 4));
end huctbrcusr;

library ieee;
use ieee.std_logic_1164.all;

architecture jeolioaxe of huctbrcusr is
  signal renfens : integer;
  signal rrzxymu : bit;
  signal fiadl : integer;
  signal pvpfkw : bit;
  signal iuzxmcyt : std_logic_vector(2 downto 0);
  signal pfozmprrc : bit;
begin
  aoqiehegz : entity work.w
    port map (ver => pfozmprrc, ygid => iuzxmcyt, lhuwf => pxkuq);
  yqyezdi : entity work.w
    port map (ver => pvpfkw, ygid => iuzxmcyt, lhuwf => fiadl);
  jxrvqaoja : entity work.w
    port map (ver => rrzxymu, ygid => iuzxmcyt, lhuwf => renfens);
  
  -- Multi-driven assignments
  iuzxmcyt <= "L-Z";
end jeolioaxe;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (huoxy : in time; asencxtz : in std_logic; tu : linkage std_logic);
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture y of f is
  signal hveoxn : integer;
  signal qbbtlqzam : std_logic_vector(2 downto 0);
  signal fwv : bit;
  signal yfeb : integer;
  signal skemfx : bit;
  signal kwgfzeo : integer;
  signal dljyzmfhxv : bit;
  signal zg : integer;
  signal tcwqfddj : std_logic_vector(2 downto 0);
  signal uqye : bit;
begin
  asous : entity work.w
    port map (ver => uqye, ygid => tcwqfddj, lhuwf => zg);
  phxbk : entity work.w
    port map (ver => dljyzmfhxv, ygid => tcwqfddj, lhuwf => kwgfzeo);
  pa : entity work.w
    port map (ver => skemfx, ygid => tcwqfddj, lhuwf => yfeb);
  apyowjz : entity work.w
    port map (ver => fwv, ygid => qbbtlqzam, lhuwf => hveoxn);
end y;

library ieee;
use ieee.std_logic_1164.all;

entity bpizmbswas is
  port (xelcvlnyk : linkage character; ugstc : inout std_logic);
end bpizmbswas;

library ieee;
use ieee.std_logic_1164.all;

architecture ugxh of bpizmbswas is
  signal poazo : std_logic;
  signal vep : time;
  signal sbbbceoe : integer;
  signal yglzuntt : std_logic_vector(2 downto 0);
  signal wcrrbfmj : bit;
begin
  ymw : entity work.w
    port map (ver => wcrrbfmj, ygid => yglzuntt, lhuwf => sbbbceoe);
  mmdz : entity work.f
    port map (huoxy => vep, asencxtz => ugstc, tu => poazo);
  
  -- Single-driven assignments
  vep <= 3_4_3_3_1 ps;
  
  -- Multi-driven assignments
  ugstc <= 'H';
  ugstc <= 'U';
  yglzuntt <= ('X', 'W', 'W');
end ugxh;



-- Seed after: 17331719985366993751,12011142928354116943
