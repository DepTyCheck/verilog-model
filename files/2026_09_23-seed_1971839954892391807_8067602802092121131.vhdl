-- Seed: 1971839954892391807,8067602802092121131

entity lqpg is
  port (akhcxaoayd : inout boolean_vector(2 downto 2); oziugm : buffer time; cctagkcv : out severity_level);
end lqpg;

architecture bvnq of lqpg is
  
begin
  
end bvnq;

library ieee;
use ieee.std_logic_1164.all;

entity tpfqypxqtt is
  port (quiqjfts : linkage std_logic; pyndgovg : in std_logic_vector(1 to 2); oobpvwdb : linkage time);
end tpfqypxqtt;

architecture anxinakbmx of tpfqypxqtt is
  signal ya : severity_level;
  signal gjbojrn : time;
  signal xgtdt : boolean_vector(2 downto 2);
  signal gvslg : severity_level;
  signal zblmzlkg : time;
  signal jbf : boolean_vector(2 downto 2);
  signal kjllzgt : severity_level;
  signal fco : time;
  signal un : boolean_vector(2 downto 2);
  signal yxbwtzd : severity_level;
  signal gq : time;
  signal bttw : boolean_vector(2 downto 2);
begin
  wvwpe : entity work.lqpg
    port map (akhcxaoayd => bttw, oziugm => gq, cctagkcv => yxbwtzd);
  hpuo : entity work.lqpg
    port map (akhcxaoayd => un, oziugm => fco, cctagkcv => kjllzgt);
  koimxzpxn : entity work.lqpg
    port map (akhcxaoayd => jbf, oziugm => zblmzlkg, cctagkcv => gvslg);
  m : entity work.lqpg
    port map (akhcxaoayd => xgtdt, oziugm => gjbojrn, cctagkcv => ya);
end anxinakbmx;

entity kef is
  port (dfzlt : out time);
end kef;

library ieee;
use ieee.std_logic_1164.all;

architecture q of kef is
  signal na : std_logic_vector(1 to 2);
  signal bnvsthen : std_logic;
  signal sjuvv : severity_level;
  signal xctkjmjr : time;
  signal mllijkhc : boolean_vector(2 downto 2);
  signal bcgbjf : severity_level;
  signal bdz : time;
  signal qafmy : boolean_vector(2 downto 2);
begin
  bj : entity work.lqpg
    port map (akhcxaoayd => qafmy, oziugm => bdz, cctagkcv => bcgbjf);
  u : entity work.lqpg
    port map (akhcxaoayd => mllijkhc, oziugm => xctkjmjr, cctagkcv => sjuvv);
  gw : entity work.tpfqypxqtt
    port map (quiqjfts => bnvsthen, pyndgovg => na, oobpvwdb => dfzlt);
  
  -- Multi-driven assignments
  na <= "WW";
  bnvsthen <= bnvsthen;
end q;

library ieee;
use ieee.std_logic_1164.all;

entity ymss is
  port (x : inout std_logic; kg : inout std_logic; wpyjtoed : buffer time);
end ymss;

architecture ighxcoayz of ymss is
  signal npbzwxb : severity_level;
  signal hbkmfb : boolean_vector(2 downto 2);
  signal wqdneepih : severity_level;
  signal suncp : time;
  signal aogh : boolean_vector(2 downto 2);
  signal xhtrncbo : severity_level;
  signal uazigdbik : time;
  signal iil : boolean_vector(2 downto 2);
begin
  chhspq : entity work.lqpg
    port map (akhcxaoayd => iil, oziugm => uazigdbik, cctagkcv => xhtrncbo);
  lfrghypxe : entity work.lqpg
    port map (akhcxaoayd => aogh, oziugm => suncp, cctagkcv => wqdneepih);
  vqtl : entity work.lqpg
    port map (akhcxaoayd => hbkmfb, oziugm => wpyjtoed, cctagkcv => npbzwxb);
  
  -- Multi-driven assignments
  x <= kg;
  kg <= x;
  kg <= x;
end ighxcoayz;



-- Seed after: 9634588949120164259,8067602802092121131
