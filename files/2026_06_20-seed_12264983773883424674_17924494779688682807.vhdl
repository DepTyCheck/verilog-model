-- Seed: 12264983773883424674,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity vhaes is
  port (knwyih : in severity_level; wvbhvps : out real; enyooy : linkage time; jfbnqdcnhb : linkage std_logic_vector(1 to 4));
end vhaes;

architecture ohukuz of vhaes is
  
begin
  -- Single-driven assignments
  wvbhvps <= 3.4_0_3_1;
end ohukuz;

library ieee;
use ieee.std_logic_1164.all;

entity stlydm is
  port (t : in std_logic_vector(0 to 4); xjmwjlpirk : in std_logic; wo : out std_logic; i : buffer integer);
end stlydm;

architecture oiddkmluap of stlydm is
  
begin
  -- Single-driven assignments
  i <= 16#2_4#;
  
  -- Multi-driven assignments
  wo <= 'U';
  wo <= 'L';
  wo <= 'H';
end oiddkmluap;

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (gowvjezpx : linkage integer; xvbdzm : linkage std_logic);
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture ciel of p is
  signal ijxfmsg : time;
  signal ruopbtpv : real;
  signal blcheepnmv : severity_level;
  signal nfmkyq : time;
  signal tblrpb : real;
  signal szltgcunty : std_logic_vector(1 to 4);
  signal vfuqe : time;
  signal t : real;
  signal ybiz : severity_level;
begin
  hoxiisncb : entity work.vhaes
    port map (knwyih => ybiz, wvbhvps => t, enyooy => vfuqe, jfbnqdcnhb => szltgcunty);
  xrdjzyflzw : entity work.vhaes
    port map (knwyih => ybiz, wvbhvps => tblrpb, enyooy => nfmkyq, jfbnqdcnhb => szltgcunty);
  ntjuhbcer : entity work.vhaes
    port map (knwyih => blcheepnmv, wvbhvps => ruopbtpv, enyooy => ijxfmsg, jfbnqdcnhb => szltgcunty);
  
  -- Single-driven assignments
  ybiz <= WARNING;
  blcheepnmv <= ERROR;
  
  -- Multi-driven assignments
  szltgcunty <= "00XW";
end ciel;



-- Seed after: 7068184497043895466,17924494779688682807
