-- Seed: 344459670279399596,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (lkjxqyczdn : buffer integer; h : in std_logic; vyklamquaz : out integer; ejhe : inout std_logic_vector(4 to 0));
end f;

architecture i of f is
  
begin
  -- Single-driven assignments
  vyklamquaz <= 2#11011#;
  lkjxqyczdn <= 4_2_4_4;
  
  -- Multi-driven assignments
  ejhe <= (others => '0');
  ejhe <= (others => '0');
  ejhe <= "";
  ejhe <= "";
end i;

library ieee;
use ieee.std_logic_1164.all;

entity nol is
  port (eckkzg : out integer; cfbkunt : buffer std_logic_vector(4 to 0); x : in boolean; fafiqlph : linkage std_logic);
end nol;

library ieee;
use ieee.std_logic_1164.all;

architecture gaenzeethe of nol is
  signal klfcvesseo : integer;
  signal hmybr : integer;
  signal jfyhl : std_logic;
  signal jiflqvpctk : integer;
  signal yd : std_logic_vector(4 to 0);
  signal nsprvbjmkt : integer;
  signal nqe : integer;
  signal yiak : std_logic_vector(4 to 0);
  signal twtjniak : integer;
  signal jqc : std_logic;
  signal jkflddy : integer;
begin
  pckel : entity work.f
    port map (lkjxqyczdn => jkflddy, h => jqc, vyklamquaz => twtjniak, ejhe => yiak);
  myuwo : entity work.f
    port map (lkjxqyczdn => nqe, h => jqc, vyklamquaz => nsprvbjmkt, ejhe => yd);
  snspquylms : entity work.f
    port map (lkjxqyczdn => jiflqvpctk, h => jfyhl, vyklamquaz => hmybr, ejhe => cfbkunt);
  vuvqr : entity work.f
    port map (lkjxqyczdn => klfcvesseo, h => jqc, vyklamquaz => eckkzg, ejhe => cfbkunt);
  
  -- Multi-driven assignments
  cfbkunt <= "";
  cfbkunt <= (others => '0');
end gaenzeethe;

entity mcw is
  port (m : out integer; tqfqwwug : out severity_level);
end mcw;

library ieee;
use ieee.std_logic_1164.all;

architecture liw of mcw is
  signal kwo : boolean;
  signal espz : integer;
  signal ygoldywxw : std_logic_vector(4 to 0);
  signal gqpb : std_logic;
  signal datfvij : integer;
begin
  fbuzojy : entity work.f
    port map (lkjxqyczdn => datfvij, h => gqpb, vyklamquaz => m, ejhe => ygoldywxw);
  ddqafjkf : entity work.nol
    port map (eckkzg => espz, cfbkunt => ygoldywxw, x => kwo, fafiqlph => gqpb);
  
  -- Single-driven assignments
  kwo <= FALSE;
  tqfqwwug <= NOTE;
  
  -- Multi-driven assignments
  ygoldywxw <= "";
  gqpb <= 'W';
  gqpb <= 'L';
end liw;

entity nukcobdot is
  port (pjt : buffer severity_level);
end nukcobdot;

library ieee;
use ieee.std_logic_1164.all;

architecture i of nukcobdot is
  signal qhhsdlkaaz : std_logic;
  signal cn : boolean;
  signal ppdpvxsjka : std_logic_vector(4 to 0);
  signal jrhao : integer;
  signal bipul : std_logic_vector(4 to 0);
  signal gt : integer;
  signal aqg : std_logic;
  signal djmfayeyn : integer;
  signal ucmx : integer;
  signal xkvlprw : severity_level;
  signal ukywhdl : integer;
begin
  dvbfxscni : entity work.mcw
    port map (m => ukywhdl, tqfqwwug => xkvlprw);
  hmoper : entity work.mcw
    port map (m => ucmx, tqfqwwug => pjt);
  dgmvdh : entity work.f
    port map (lkjxqyczdn => djmfayeyn, h => aqg, vyklamquaz => gt, ejhe => bipul);
  pgdc : entity work.nol
    port map (eckkzg => jrhao, cfbkunt => ppdpvxsjka, x => cn, fafiqlph => qhhsdlkaaz);
  
  -- Multi-driven assignments
  ppdpvxsjka <= (others => '0');
  aqg <= 'Z';
end i;



-- Seed after: 17808733375477500599,5472058987609252853
