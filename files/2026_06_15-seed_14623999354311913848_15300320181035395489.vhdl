-- Seed: 14623999354311913848,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity obj is
  port (zjoxiucsz : linkage std_logic; vcfmcwjgp : in string(1 downto 4); ge : linkage severity_level; vqnbhdllsi : in time);
end obj;

architecture eufadopuy of obj is
  
begin
  
end eufadopuy;

library ieee;
use ieee.std_logic_1164.all;

entity ihrdhnzgdw is
  port (cktbalh : out time; ozhgdu : inout real; hkiyemogf : buffer real_vector(0 to 1); szul : out std_logic_vector(4 to 3));
end ihrdhnzgdw;

architecture mvxauqcaju of ihrdhnzgdw is
  
begin
  -- Multi-driven assignments
  szul <= "";
end mvxauqcaju;

library ieee;
use ieee.std_logic_1164.all;

entity oohs is
  port (i : inout character; rwvft : inout boolean_vector(2 downto 1); itobq : in real; yoyh : buffer std_logic_vector(1 downto 0));
end oohs;

library ieee;
use ieee.std_logic_1164.all;

architecture utkfkn of oohs is
  signal fojp : severity_level;
  signal xubxvkp : time;
  signal wfwzcqnjo : severity_level;
  signal ztmf : string(1 downto 4);
  signal hbxrbgtsvg : std_logic;
  signal heqldmirj : real_vector(0 to 1);
  signal fj : real;
  signal x : time;
  signal ta : std_logic_vector(4 to 3);
  signal gtmemb : real_vector(0 to 1);
  signal yd : real;
  signal fiprppo : time;
begin
  chfqsfs : entity work.ihrdhnzgdw
    port map (cktbalh => fiprppo, ozhgdu => yd, hkiyemogf => gtmemb, szul => ta);
  coyguiaoyh : entity work.ihrdhnzgdw
    port map (cktbalh => x, ozhgdu => fj, hkiyemogf => heqldmirj, szul => ta);
  vfdyr : entity work.obj
    port map (zjoxiucsz => hbxrbgtsvg, vcfmcwjgp => ztmf, ge => wfwzcqnjo, vqnbhdllsi => xubxvkp);
  ryserzkerw : entity work.obj
    port map (zjoxiucsz => hbxrbgtsvg, vcfmcwjgp => ztmf, ge => fojp, vqnbhdllsi => xubxvkp);
  
  -- Single-driven assignments
  rwvft <= (FALSE, TRUE);
  i <= 'e';
  
  -- Multi-driven assignments
  yoyh <= ('Z', '1');
  yoyh <= ('U', '0');
end utkfkn;



-- Seed after: 8160946736376045602,15300320181035395489
