-- Seed: 11713991943054832204,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity jxtsebseky is
  port (yzppn : linkage std_logic; xehgvdwu : in std_logic);
end jxtsebseky;

architecture hhaszu of jxtsebseky is
  
begin
  
end hhaszu;

entity fpowad is
  port (l : buffer boolean_vector(1 to 3); vrlnmauqr : linkage time_vector(1 downto 1); aicwjfs : in integer);
end fpowad;

library ieee;
use ieee.std_logic_1164.all;

architecture mnkpawbbqv of fpowad is
  signal rpbrg : std_logic;
  signal ljllxi : std_logic;
  signal iaxzbt : std_logic;
begin
  itedby : entity work.jxtsebseky
    port map (yzppn => iaxzbt, xehgvdwu => ljllxi);
  akjsxtqcwj : entity work.jxtsebseky
    port map (yzppn => rpbrg, xehgvdwu => iaxzbt);
  
  -- Multi-driven assignments
  iaxzbt <= rpbrg;
  rpbrg <= '-';
  iaxzbt <= 'U';
  ljllxi <= iaxzbt;
end mnkpawbbqv;

entity woujyffwka is
  port (aihcab : buffer bit; egbxbgwnem : out time; etz : in real);
end woujyffwka;

library ieee;
use ieee.std_logic_1164.all;

architecture z of woujyffwka is
  signal pndwpfls : std_logic;
  signal zpoktmsy : std_logic;
  signal pdpwdlfsay : integer;
  signal ueiwwg : time_vector(1 downto 1);
  signal rr : boolean_vector(1 to 3);
begin
  akqdnj : entity work.fpowad
    port map (l => rr, vrlnmauqr => ueiwwg, aicwjfs => pdpwdlfsay);
  clvx : entity work.jxtsebseky
    port map (yzppn => zpoktmsy, xehgvdwu => pndwpfls);
  
  -- Single-driven assignments
  egbxbgwnem <= egbxbgwnem;
  pdpwdlfsay <= 8#1_5_6#;
  aihcab <= '0';
  
  -- Multi-driven assignments
  zpoktmsy <= 'L';
  zpoktmsy <= 'Z';
end z;

library ieee;
use ieee.std_logic_1164.all;

entity nfxnryxp is
  port (odferihcy : out std_logic_vector(4 to 4));
end nfxnryxp;

library ieee;
use ieee.std_logic_1164.all;

architecture wamhxkjbv of nfxnryxp is
  signal loxnyzsv : integer;
  signal g : time_vector(1 downto 1);
  signal sldjojxy : boolean_vector(1 to 3);
  signal pvbxgbi : std_logic;
  signal jycqma : std_logic;
begin
  ufjhsmxgta : entity work.jxtsebseky
    port map (yzppn => jycqma, xehgvdwu => pvbxgbi);
  fch : entity work.fpowad
    port map (l => sldjojxy, vrlnmauqr => g, aicwjfs => loxnyzsv);
end wamhxkjbv;



-- Seed after: 14857758325881767803,9900838946149614537
