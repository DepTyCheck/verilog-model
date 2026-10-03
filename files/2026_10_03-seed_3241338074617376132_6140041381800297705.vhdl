-- Seed: 3241338074617376132,6140041381800297705

library ieee;
use ieee.std_logic_1164.all;

entity tllr is
  port (osiyfzto : in std_logic_vector(4 to 3); ydhb : linkage std_logic_vector(1 downto 4));
end tllr;

architecture icqzcllo of tllr is
  
begin
  
end icqzcllo;

library ieee;
use ieee.std_logic_1164.all;

entity fw is
  port (p : out std_logic; qaelisewc : buffer time; kogkttzd : inout std_logic);
end fw;

library ieee;
use ieee.std_logic_1164.all;

architecture jytmu of fw is
  signal rkla : std_logic_vector(1 downto 4);
  signal hpx : std_logic_vector(4 to 3);
begin
  aakmnr : entity work.tllr
    port map (osiyfzto => hpx, ydhb => rkla);
  
  -- Single-driven assignments
  qaelisewc <= qaelisewc;
  
  -- Multi-driven assignments
  kogkttzd <= kogkttzd;
  kogkttzd <= 'U';
end jytmu;

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (hzowtn : inout character; u : in std_logic; twwnendb : buffer severity_level);
end i;

library ieee;
use ieee.std_logic_1164.all;

architecture wablijjq of i is
  signal jcxyuts : std_logic_vector(1 downto 4);
  signal frwquefd : time;
  signal qtl : std_logic;
  signal zankurnhv : std_logic_vector(1 downto 4);
  signal jlad : std_logic_vector(4 to 3);
  signal oxt : std_logic_vector(4 to 3);
begin
  nnm : entity work.tllr
    port map (osiyfzto => oxt, ydhb => oxt);
  zjvymn : entity work.tllr
    port map (osiyfzto => jlad, ydhb => zankurnhv);
  pcd : entity work.fw
    port map (p => qtl, qaelisewc => frwquefd, kogkttzd => qtl);
  dzpcqnqxnf : entity work.tllr
    port map (osiyfzto => oxt, ydhb => jcxyuts);
  
  -- Single-driven assignments
  twwnendb <= twwnendb;
  hzowtn <= 'e';
  
  -- Multi-driven assignments
  oxt <= oxt;
  oxt <= oxt;
  oxt <= (others => '0');
end wablijjq;



-- Seed after: 8695824645320000106,6140041381800297705
