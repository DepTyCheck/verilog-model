-- Seed: 6227331250274977516,6140041381800297705

library ieee;
use ieee.std_logic_1164.all;

entity sksazfw is
  port (pt : buffer std_logic);
end sksazfw;

architecture g of sksazfw is
  
begin
  -- Multi-driven assignments
  pt <= '0';
  pt <= 'L';
  pt <= pt;
  pt <= 'W';
end g;

library ieee;
use ieee.std_logic_1164.all;

entity la is
  port (jbs : out string(1 downto 1); xwx : out character; gbaoutgtss : buffer std_logic; npnadxi : in integer);
end la;

library ieee;
use ieee.std_logic_1164.all;

architecture mu of la is
  signal imayudcm : std_logic;
begin
  bq : entity work.sksazfw
    port map (pt => imayudcm);
  btv : entity work.sksazfw
    port map (pt => gbaoutgtss);
  ct : entity work.sksazfw
    port map (pt => gbaoutgtss);
  
  -- Single-driven assignments
  xwx <= 'd';
  jbs <= (others => 'x');
  
  -- Multi-driven assignments
  gbaoutgtss <= 'U';
  imayudcm <= imayudcm;
end mu;

library ieee;
use ieee.std_logic_1164.all;

entity jcxb is
  port ( tyxr : linkage real_vector(1 to 3)
  ; pn : inout std_logic_vector(3 downto 1)
  ; fbhfxcmfs : out std_logic_vector(3 downto 2)
  ; ektpmoalxh : inout std_logic
  );
end jcxb;

architecture cq of jcxb is
  
begin
  v : entity work.sksazfw
    port map (pt => ektpmoalxh);
  
  -- Multi-driven assignments
  ektpmoalxh <= 'U';
  ektpmoalxh <= ektpmoalxh;
end cq;



-- Seed after: 4688206244918652599,6140041381800297705
