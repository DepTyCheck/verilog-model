-- Seed: 5123780134593461815,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity duub is
  port (movamrvrfn : linkage time_vector(0 downto 1); smr : out std_logic_vector(1 to 0); dpdg : buffer time);
end duub;

architecture i of duub is
  
begin
  -- Single-driven assignments
  dpdg <= 8#3# ps;
  
  -- Multi-driven assignments
  smr <= (others => '0');
  smr <= (others => '0');
  smr <= "";
  smr <= "";
end i;

library ieee;
use ieee.std_logic_1164.all;

entity jzggjfsz is
  port (qxruhu : out integer_vector(4 to 4); xkckvyie : inout std_logic; hfv : out real);
end jzggjfsz;

library ieee;
use ieee.std_logic_1164.all;

architecture fbalrhd of jzggjfsz is
  signal rdot : time;
  signal lcl : std_logic_vector(1 to 0);
  signal zxyjzthvf : time_vector(0 downto 1);
begin
  jc : entity work.duub
    port map (movamrvrfn => zxyjzthvf, smr => lcl, dpdg => rdot);
  
  -- Single-driven assignments
  hfv <= 2#01100.011#;
  qxruhu <= (others => 8#0_6_7_4_2#);
  
  -- Multi-driven assignments
  xkckvyie <= 'H';
  xkckvyie <= '-';
  xkckvyie <= 'Z';
  lcl <= "";
end fbalrhd;

entity psinp is
  port (xwqbkl : out integer);
end psinp;

library ieee;
use ieee.std_logic_1164.all;

architecture zogv of psinp is
  signal fdili : time;
  signal pmz : std_logic_vector(1 to 0);
  signal lq : time_vector(0 downto 1);
begin
  eumkaoy : entity work.duub
    port map (movamrvrfn => lq, smr => pmz, dpdg => fdili);
  
  -- Multi-driven assignments
  pmz <= (others => '0');
end zogv;



-- Seed after: 6842858017040857134,8118127366649987907
