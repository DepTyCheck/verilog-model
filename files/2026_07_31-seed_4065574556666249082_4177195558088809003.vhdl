-- Seed: 4065574556666249082,4177195558088809003

entity rscakyw is
  port (kepdwiyemm : in real);
end rscakyw;

architecture y of rscakyw is
  
begin
  
end y;

library ieee;
use ieee.std_logic_1164.all;

entity nh is
  port (lgtewxxv : buffer time; cvyen : out severity_level; btvuteag : out std_logic_vector(1 to 2); wo : inout real);
end nh;

architecture fpmq of nh is
  
begin
  tbvq : entity work.rscakyw
    port map (kepdwiyemm => wo);
  
  -- Single-driven assignments
  cvyen <= ERROR;
  lgtewxxv <= lgtewxxv;
  wo <= wo;
end fpmq;

entity mvylfjgcs is
  port (eoljz : out real; hsalnmtc : inout bit; qw : in time);
end mvylfjgcs;

architecture igicfuq of mvylfjgcs is
  
begin
  fvtzerkdo : entity work.rscakyw
    port map (kepdwiyemm => eoljz);
end igicfuq;

library ieee;
use ieee.std_logic_1164.all;

entity ufiufasme is
  port (nlsc : linkage boolean; fwyzw : in std_logic; ngxz : in real);
end ufiufasme;

library ieee;
use ieee.std_logic_1164.all;

architecture xptup of ufiufasme is
  signal bpthmfk : real;
  signal kkez : std_logic_vector(1 to 2);
  signal wq : severity_level;
  signal emqfqsc : time;
  signal jeii : time;
  signal vgcdssxki : bit;
  signal kgl : real;
begin
  pthkgt : entity work.rscakyw
    port map (kepdwiyemm => ngxz);
  imiwysd : entity work.rscakyw
    port map (kepdwiyemm => kgl);
  pyddxyrg : entity work.mvylfjgcs
    port map (eoljz => kgl, hsalnmtc => vgcdssxki, qw => jeii);
  qhmnkknxyi : entity work.nh
    port map (lgtewxxv => emqfqsc, cvyen => wq, btvuteag => kkez, wo => bpthmfk);
  
  -- Single-driven assignments
  jeii <= 20000 ms;
  
  -- Multi-driven assignments
  kkez <= kkez;
  kkez <= "1L";
end xptup;



-- Seed after: 3597780963158140191,4177195558088809003
