-- Seed: 17949360358239345567,3042374792655995433

library ieee;
use ieee.std_logic_1164.all;

entity tnugt is
  port (qnwbzov : in real; bqaam : linkage std_logic_vector(3 to 4); fhtq : in time; hyj : inout real);
end tnugt;

architecture aujcty of tnugt is
  
begin
  -- Single-driven assignments
  hyj <= 312.2_3_4_4_4;
end aujcty;

entity skap is
  port (beg : out real; jvph : inout real);
end skap;

library ieee;
use ieee.std_logic_1164.all;

architecture gmcohepqa of skap is
  signal sfyplytm : time;
  signal rhqslv : std_logic_vector(3 to 4);
begin
  bdotnfvlh : entity work.tnugt
    port map (qnwbzov => beg, bqaam => rhqslv, fhtq => sfyplytm, hyj => jvph);
  
  -- Single-driven assignments
  beg <= 16#647F7.7#;
  sfyplytm <= 1 us;
  
  -- Multi-driven assignments
  rhqslv <= ('0', 'U');
  rhqslv <= rhqslv;
  rhqslv <= rhqslv;
  rhqslv <= "UX";
end gmcohepqa;

entity uhqwj is
  port (vtiktmrd : in real_vector(1 downto 3); twayw : out real_vector(3 to 0); guvepn : linkage integer);
end uhqwj;

library ieee;
use ieee.std_logic_1164.all;

architecture iuuqbvfjig of uhqwj is
  signal bgpflyab : real;
  signal uakxl : real;
  signal gjt : time;
  signal kvfkh : std_logic_vector(3 to 4);
  signal zkedseath : real;
begin
  x : entity work.tnugt
    port map (qnwbzov => zkedseath, bqaam => kvfkh, fhtq => gjt, hyj => uakxl);
  zemip : entity work.tnugt
    port map (qnwbzov => zkedseath, bqaam => kvfkh, fhtq => gjt, hyj => bgpflyab);
  
  -- Single-driven assignments
  twayw <= (others => 0.0);
  gjt <= 1_4_3_4_0.033 fs;
  zkedseath <= uakxl;
end iuuqbvfjig;

library ieee;
use ieee.std_logic_1164.all;

entity uvxaoo is
  port (xxioroji : in time_vector(1 to 2); mlzstzulky : in real; b : buffer integer; yhzjrkew : linkage std_logic_vector(2 to 3));
end uvxaoo;

architecture gcuq of uvxaoo is
  signal hhxofj : real;
  signal bl : integer;
  signal ovvhtlkhq : real_vector(3 to 0);
  signal llkzk : real;
  signal vnarbuc : time;
  signal yslisik : real;
begin
  bp : entity work.tnugt
    port map (qnwbzov => yslisik, bqaam => yhzjrkew, fhtq => vnarbuc, hyj => llkzk);
  doocqga : entity work.uhqwj
    port map (vtiktmrd => ovvhtlkhq, twayw => ovvhtlkhq, guvepn => bl);
  bkw : entity work.skap
    port map (beg => hhxofj, jvph => yslisik);
  
  -- Single-driven assignments
  b <= bl;
  vnarbuc <= 16#5176.2_B_D_A# ns;
end gcuq;



-- Seed after: 14457183228161342052,3042374792655995433
