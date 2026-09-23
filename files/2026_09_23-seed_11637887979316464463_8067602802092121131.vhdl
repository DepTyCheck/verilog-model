-- Seed: 11637887979316464463,8067602802092121131

entity lkid is
  port (yexqhdo : in string(4 to 2); x : inout time_vector(3 to 2); axaj : in severity_level; p : in real);
end lkid;

architecture dmwqjtca of lkid is
  
begin
  -- Single-driven assignments
  x <= (others => 0 ns);
end dmwqjtca;

library ieee;
use ieee.std_logic_1164.all;

entity boroy is
  port (sbxvvfb : inout severity_level; hwgojux : in std_logic_vector(0 downto 4); seq : linkage std_logic_vector(3 downto 0));
end boroy;

architecture rhyazkrle of boroy is
  signal ths : severity_level;
  signal btr : time_vector(3 to 2);
  signal a : real;
  signal pysknmr : severity_level;
  signal e : time_vector(3 to 2);
  signal xggdxgwhim : string(4 to 2);
begin
  qqauq : entity work.lkid
    port map (yexqhdo => xggdxgwhim, x => e, axaj => pysknmr, p => a);
  qerhwpv : entity work.lkid
    port map (yexqhdo => xggdxgwhim, x => btr, axaj => ths, p => a);
  
  -- Single-driven assignments
  sbxvvfb <= ths;
  xggdxgwhim <= "";
end rhyazkrle;

library ieee;
use ieee.std_logic_1164.all;

entity hycqgp is
  port (toxlktc : buffer std_logic_vector(0 to 2); k : buffer time; gecikzq : out boolean);
end hycqgp;

library ieee;
use ieee.std_logic_1164.all;

architecture upv of hycqgp is
  signal ucu : std_logic_vector(3 downto 0);
  signal yxtcrl : std_logic_vector(0 downto 4);
  signal pskxtadynz : severity_level;
begin
  vgknixdl : entity work.boroy
    port map (sbxvvfb => pskxtadynz, hwgojux => yxtcrl, seq => ucu);
  
  -- Single-driven assignments
  gecikzq <= TRUE;
  k <= k;
  
  -- Multi-driven assignments
  toxlktc <= ('W', 'W', '-');
  ucu <= ucu;
  ucu <= ucu;
  toxlktc <= toxlktc;
end upv;

library ieee;
use ieee.std_logic_1164.all;

entity lecyek is
  port (uqjokw : buffer std_logic; md : out std_logic; vmjsjkefkq : buffer real; ispuhsipe : buffer integer);
end lecyek;

architecture aqqo of lecyek is
  signal gezv : real;
  signal dlf : severity_level;
  signal j : time_vector(3 to 2);
  signal iv : string(4 to 2);
  signal waqzgwf : time_vector(3 to 2);
  signal plhdyspo : string(4 to 2);
  signal unomabi : severity_level;
  signal oanw : time_vector(3 to 2);
  signal ajbtklden : string(4 to 2);
begin
  hv : entity work.lkid
    port map (yexqhdo => ajbtklden, x => oanw, axaj => unomabi, p => vmjsjkefkq);
  zsesujsdlg : entity work.lkid
    port map (yexqhdo => plhdyspo, x => waqzgwf, axaj => unomabi, p => vmjsjkefkq);
  cqimqwl : entity work.lkid
    port map (yexqhdo => iv, x => j, axaj => dlf, p => gezv);
  
  -- Multi-driven assignments
  md <= uqjokw;
end aqqo;



-- Seed after: 1971839954892391807,8067602802092121131
