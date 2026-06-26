-- Seed: 6893633022123123612,12011142928354116943

entity qgmjqzlq is
  port (xenm : buffer integer; dmfvbi : in time);
end qgmjqzlq;

architecture rb of qgmjqzlq is
  
begin
  -- Single-driven assignments
  xenm <= 2#011#;
end rb;

entity fy is
  port (llvshdwt : buffer integer);
end fy;

architecture lsqub of fy is
  signal xfdalxrwhy : time;
  signal qzqararih : integer;
  signal oocoihqyox : time;
begin
  epkm : entity work.qgmjqzlq
    port map (xenm => llvshdwt, dmfvbi => oocoihqyox);
  hfzvpip : entity work.qgmjqzlq
    port map (xenm => qzqararih, dmfvbi => xfdalxrwhy);
  
  -- Single-driven assignments
  oocoihqyox <= 16#A# ms;
  xfdalxrwhy <= 16#6# ms;
end lsqub;

library ieee;
use ieee.std_logic_1164.all;

entity zrsiw is
  port (mxuzljtn : out std_logic_vector(2 to 2); sstpmbuuh : out time; hcg : out real; ix : buffer time);
end zrsiw;

architecture jikkkfyl of zrsiw is
  signal yfmedhfhit : time;
  signal rrbp : integer;
begin
  ydve : entity work.qgmjqzlq
    port map (xenm => rrbp, dmfvbi => yfmedhfhit);
  
  -- Single-driven assignments
  hcg <= 8#1_3_5.4_1_6#;
  ix <= 0.0_3_4_4_1 ns;
  
  -- Multi-driven assignments
  mxuzljtn <= (others => 'U');
  mxuzljtn <= "X";
  mxuzljtn <= (others => '0');
end jikkkfyl;



-- Seed after: 11843033271027059599,12011142928354116943
