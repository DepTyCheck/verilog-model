-- Seed: 11518224073262980998,3042374792655995433

library ieee;
use ieee.std_logic_1164.all;

entity vlgcp is
  port (uknpcvfpni : inout std_logic; rczmlzfse : inout bit; tiznwld : buffer real; wgkxuze : inout boolean);
end vlgcp;

architecture lgytgeicew of vlgcp is
  
begin
  -- Single-driven assignments
  rczmlzfse <= '1';
  tiznwld <= tiznwld;
  wgkxuze <= TRUE;
  
  -- Multi-driven assignments
  uknpcvfpni <= uknpcvfpni;
  uknpcvfpni <= uknpcvfpni;
  uknpcvfpni <= uknpcvfpni;
end lgytgeicew;

entity vfjyuxrdj is
  port (oqpb : buffer time_vector(1 downto 1); hgqn : linkage time);
end vfjyuxrdj;

architecture aqypb of vfjyuxrdj is
  
begin
  -- Single-driven assignments
  oqpb <= (others => 141 ps);
end aqypb;

entity cz is
  port (ucmicew : linkage real; s : in severity_level);
end cz;

library ieee;
use ieee.std_logic_1164.all;

architecture kd of cz is
  signal ucypw : boolean;
  signal iiver : real;
  signal vpmkk : bit;
  signal e : time;
  signal umcxtwhruf : time_vector(1 downto 1);
  signal rirexry : boolean;
  signal dyw : real;
  signal eujexcqqu : bit;
  signal fyfcuxcwi : std_logic;
begin
  zhinao : entity work.vlgcp
    port map (uknpcvfpni => fyfcuxcwi, rczmlzfse => eujexcqqu, tiznwld => dyw, wgkxuze => rirexry);
  wrvtyvv : entity work.vfjyuxrdj
    port map (oqpb => umcxtwhruf, hgqn => e);
  nhbvwrli : entity work.vlgcp
    port map (uknpcvfpni => fyfcuxcwi, rczmlzfse => vpmkk, tiznwld => iiver, wgkxuze => ucypw);
  
  -- Multi-driven assignments
  fyfcuxcwi <= '1';
end kd;

entity wf is
  port (kccvrts : in bit; j : inout character; ktec : inout time; cruavzxojr : linkage boolean);
end wf;

library ieee;
use ieee.std_logic_1164.all;

architecture p of wf is
  signal bv : severity_level;
  signal uo : real;
  signal xgkguoj : boolean;
  signal wuxpwce : real;
  signal rtueiipsz : bit;
  signal syovypq : std_logic;
  signal pkdbltk : boolean;
  signal akdgavs : real;
  signal saxbkynvv : bit;
  signal vmvygbrdhu : boolean;
  signal l : real;
  signal itiibn : bit;
  signal fm : std_logic;
begin
  wqthl : entity work.vlgcp
    port map (uknpcvfpni => fm, rczmlzfse => itiibn, tiznwld => l, wgkxuze => vmvygbrdhu);
  eznds : entity work.vlgcp
    port map (uknpcvfpni => fm, rczmlzfse => saxbkynvv, tiznwld => akdgavs, wgkxuze => pkdbltk);
  ra : entity work.vlgcp
    port map (uknpcvfpni => syovypq, rczmlzfse => rtueiipsz, tiznwld => wuxpwce, wgkxuze => xgkguoj);
  sfixbedc : entity work.cz
    port map (ucmicew => uo, s => bv);
  
  -- Single-driven assignments
  j <= 'b';
  ktec <= 2#01001.00# ns;
  
  -- Multi-driven assignments
  fm <= fm;
  fm <= '0';
  fm <= fm;
  fm <= 'L';
end p;



-- Seed after: 17402192367316750627,3042374792655995433
