-- Seed: 16738683438866478135,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity fvbxjsodt is
  port (l : buffer integer; ioxzzwaw : in std_logic_vector(2 to 2); lebof : in std_logic_vector(4 downto 3); ndzuvdtaxe : linkage integer);
end fvbxjsodt;

architecture irgmuptwa of fvbxjsodt is
  
begin
  -- Single-driven assignments
  l <= l;
end irgmuptwa;

library ieee;
use ieee.std_logic_1164.all;

entity tjq is
  port (h : buffer integer; nomdd : in integer; c : buffer std_logic_vector(1 downto 0));
end tjq;

library ieee;
use ieee.std_logic_1164.all;

architecture bvyo of tjq is
  signal pm : std_logic_vector(4 downto 3);
  signal mcpwisrwuc : integer;
  signal ryg : integer;
  signal euu : std_logic_vector(4 downto 3);
  signal as : std_logic_vector(2 to 2);
  signal bqzhmbs : integer;
begin
  mpgvxnrja : entity work.fvbxjsodt
    port map (l => bqzhmbs, ioxzzwaw => as, lebof => euu, ndzuvdtaxe => ryg);
  mvpc : entity work.fvbxjsodt
    port map (l => mcpwisrwuc, ioxzzwaw => as, lebof => pm, ndzuvdtaxe => h);
end bvyo;

library ieee;
use ieee.std_logic_1164.all;

entity dbbedpnhp is
  port (yhnp : in integer_vector(4 to 4); vfjcvai : linkage time; ryobdyha : linkage character; s : linkage std_logic);
end dbbedpnhp;

library ieee;
use ieee.std_logic_1164.all;

architecture u of dbbedpnhp is
  signal n : integer;
  signal ymbxfdkzaj : integer;
  signal rd : std_logic_vector(1 downto 0);
  signal lzotw : integer;
begin
  sxajonm : entity work.tjq
    port map (h => lzotw, nomdd => lzotw, c => rd);
  kedzoi : entity work.tjq
    port map (h => ymbxfdkzaj, nomdd => n, c => rd);
  
  -- Single-driven assignments
  n <= 4;
end u;

entity cgdhjzxsq is
  port (ebqckdrn : out boolean);
end cgdhjzxsq;

library ieee;
use ieee.std_logic_1164.all;

architecture tjqcm of cgdhjzxsq is
  signal frzfqigatg : std_logic;
  signal skzf : character;
  signal cedswhkwa : time;
  signal qeeqbe : integer_vector(4 to 4);
  signal edzcvgz : std_logic_vector(1 downto 0);
  signal liwafyk : integer;
  signal nodsz : integer;
  signal sqzyxekhbt : integer;
  signal pewpmqtsni : std_logic_vector(4 downto 3);
  signal y : std_logic_vector(2 to 2);
  signal ldtbilvud : std_logic_vector(1 downto 0);
  signal sgrcdtuao : integer;
  signal mdlnfmmm : integer;
begin
  xrdox : entity work.tjq
    port map (h => mdlnfmmm, nomdd => sgrcdtuao, c => ldtbilvud);
  rtmdj : entity work.fvbxjsodt
    port map (l => sgrcdtuao, ioxzzwaw => y, lebof => pewpmqtsni, ndzuvdtaxe => sqzyxekhbt);
  wxwnniwki : entity work.tjq
    port map (h => nodsz, nomdd => liwafyk, c => edzcvgz);
  ngyhag : entity work.dbbedpnhp
    port map (yhnp => qeeqbe, vfjcvai => cedswhkwa, ryobdyha => skzf, s => frzfqigatg);
  
  -- Single-driven assignments
  ebqckdrn <= TRUE;
  qeeqbe <= (others => 2#1_1_1_0#);
  liwafyk <= mdlnfmmm;
  
  -- Multi-driven assignments
  frzfqigatg <= '0';
  pewpmqtsni <= ldtbilvud;
end tjqcm;



-- Seed after: 122456334055288504,3316342841050048249
