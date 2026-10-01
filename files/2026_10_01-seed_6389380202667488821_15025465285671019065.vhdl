-- Seed: 6389380202667488821,15025465285671019065

entity svrvk is
  port (iyb : inout real_vector(4 downto 1));
end svrvk;

architecture iigci of svrvk is
  
begin
  -- Single-driven assignments
  iyb <= (3_1_2_4.4_0_2_2_0, 2#0.0_0#, 1_3_3_4_4.12033, 0_4.2);
end iigci;

entity fxqsskofy is
  port (yujwqe : inout time; qs : out real_vector(1 to 4));
end fxqsskofy;

architecture jdmqqketc of fxqsskofy is
  
begin
  yoz : entity work.svrvk
    port map (iyb => qs);
  
  -- Single-driven assignments
  yujwqe <= 0_3 ms;
end jdmqqketc;

library ieee;
use ieee.std_logic_1164.all;

entity evpyxisd is
  port (z : in std_logic_vector(0 downto 0); k : linkage real; m : in std_logic_vector(2 to 1));
end evpyxisd;

architecture e of evpyxisd is
  signal xdhvr : real_vector(1 to 4);
  signal ebnrncy : time;
  signal yjrrojfdrt : real_vector(4 downto 1);
begin
  ikrqugexc : entity work.svrvk
    port map (iyb => yjrrojfdrt);
  jodrzqwlaa : entity work.fxqsskofy
    port map (yujwqe => ebnrncy, qs => xdhvr);
end e;

entity zdnhyyg is
  port (mignejvb : inout time; tfbicl : in boolean_vector(1 downto 3));
end zdnhyyg;

architecture rovgva of zdnhyyg is
  signal dugf : real_vector(1 to 4);
begin
  dw : entity work.fxqsskofy
    port map (yujwqe => mignejvb, qs => dugf);
end rovgva;



-- Seed after: 13671504582420591260,15025465285671019065
