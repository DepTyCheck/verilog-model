-- Seed: 497909592305981700,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity ssrl is
  port (aorkgcsn : in time; ffhvjk : in std_logic_vector(0 to 2); ve : out time);
end ssrl;

architecture k of ssrl is
  
begin
  -- Single-driven assignments
  ve <= 0331.44 ms;
end k;

library ieee;
use ieee.std_logic_1164.all;

entity naohvag is
  port (jduhjbnzo : out time; izygkvtrjd : inout real; jbbmponeen : in std_logic_vector(2 to 1); tgoeguh : linkage std_logic_vector(3 to 2));
end naohvag;

architecture ur of naohvag is
  
begin
  -- Single-driven assignments
  izygkvtrjd <= 1_0_0_2.140;
  jduhjbnzo <= 1 sec;
end ur;

library ieee;
use ieee.std_logic_1164.all;

entity kuomti is
  port (yoslfy : buffer real; iltnurmk : out time_vector(1 to 1); tchhfcee : in std_logic);
end kuomti;

library ieee;
use ieee.std_logic_1164.all;

architecture zqfxdozzk of kuomti is
  signal nofmtyppl : std_logic_vector(3 to 2);
  signal uuzzraty : std_logic_vector(2 to 1);
  signal cxfr : real;
  signal magvd : std_logic_vector(0 to 2);
  signal dilhyvar : time;
  signal eclkw : time;
  signal rs : std_logic_vector(0 to 2);
  signal bmpoq : time;
begin
  co : entity work.ssrl
    port map (aorkgcsn => bmpoq, ffhvjk => rs, ve => eclkw);
  jazxnr : entity work.ssrl
    port map (aorkgcsn => dilhyvar, ffhvjk => magvd, ve => bmpoq);
  cdgwnz : entity work.naohvag
    port map (jduhjbnzo => dilhyvar, izygkvtrjd => cxfr, jbbmponeen => uuzzraty, tgoeguh => nofmtyppl);
  
  -- Single-driven assignments
  iltnurmk <= iltnurmk;
  yoslfy <= yoslfy;
end zqfxdozzk;

library ieee;
use ieee.std_logic_1164.all;

entity fcl is
  port (ogyoiid : in std_logic; coozg : out std_logic; vqscl : in real; cyqhmybcxv : in time);
end fcl;

architecture coqfpt of fcl is
  
begin
  -- Multi-driven assignments
  coozg <= 'X';
  coozg <= coozg;
  coozg <= coozg;
  coozg <= 'L';
end coqfpt;



-- Seed after: 5542233516098274588,14108379317686958507
