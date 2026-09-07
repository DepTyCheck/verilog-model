-- Seed: 11893894881156226376,12269339630485015285

entity x is
  port (teasgz : inout real; q : linkage real_vector(1 downto 3); saezkn : in real_vector(0 downto 0); dqjpumr : buffer time);
end x;

architecture pzsm of x is
  
begin
  -- Single-driven assignments
  dqjpumr <= 44343 ms;
  teasgz <= 2#1.1_1_1_0#;
end pzsm;

entity utiknzdlk is
  port (oymjldzklp : buffer real_vector(0 downto 2); mnrfxtqntw : inout real; antb : linkage string(3 downto 3));
end utiknzdlk;

architecture ieqghb of utiknzdlk is
  signal wcbzxu : time;
  signal jmlxtlr : real_vector(0 downto 0);
  signal gyysgast : real_vector(1 downto 3);
  signal gjbiarzylf : time;
  signal pxvbze : real_vector(0 downto 0);
  signal gcz : real;
begin
  rfnfn : entity work.x
    port map (teasgz => gcz, q => oymjldzklp, saezkn => pxvbze, dqjpumr => gjbiarzylf);
  r : entity work.x
    port map (teasgz => mnrfxtqntw, q => gyysgast, saezkn => jmlxtlr, dqjpumr => wcbzxu);
end ieqghb;

library ieee;
use ieee.std_logic_1164.all;

entity yoyjhsbip is
  port (maszxmuk : in time; vojx : in std_logic);
end yoyjhsbip;

architecture mno of yoyjhsbip is
  signal begbpw : time;
  signal rhtbnuardf : real_vector(1 downto 3);
  signal ezxvvzys : real;
  signal nzqhf : time;
  signal h : real_vector(0 downto 0);
  signal euvrbhj : real_vector(1 downto 3);
  signal cmhqm : real;
begin
  cq : entity work.x
    port map (teasgz => cmhqm, q => euvrbhj, saezkn => h, dqjpumr => nzqhf);
  izagsmkn : entity work.x
    port map (teasgz => ezxvvzys, q => rhtbnuardf, saezkn => h, dqjpumr => begbpw);
  
  -- Single-driven assignments
  h <= (others => 16#0_B.1_C#);
end mno;



-- Seed after: 11305781241540679515,12269339630485015285
