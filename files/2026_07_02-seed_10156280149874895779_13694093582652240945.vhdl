-- Seed: 10156280149874895779,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity aomxge is
  port (qlzng : linkage std_logic; udyyhaczc : inout time);
end aomxge;

architecture tv of aomxge is
  
begin
  -- Single-driven assignments
  udyyhaczc <= 8#4722.6_2_4_4# ps;
end tv;

entity q is
  port (jelxonke : inout time; taq : linkage real_vector(2 downto 0));
end q;

library ieee;
use ieee.std_logic_1164.all;

architecture cfjmvg of q is
  signal qsrq : time;
  signal pkoqvhptp : time;
  signal kogormjx : std_logic;
  signal nzzeidggdb : time;
  signal dq : std_logic;
begin
  pxgsigojqk : entity work.aomxge
    port map (qlzng => dq, udyyhaczc => nzzeidggdb);
  bqoivzpuv : entity work.aomxge
    port map (qlzng => kogormjx, udyyhaczc => pkoqvhptp);
  ozlboguu : entity work.aomxge
    port map (qlzng => dq, udyyhaczc => qsrq);
  
  -- Single-driven assignments
  jelxonke <= 2#0_0_0# ms;
  
  -- Multi-driven assignments
  dq <= 'Z';
  dq <= 'U';
end cfjmvg;



-- Seed after: 7512563358547121027,13694093582652240945
