-- Seed: 14569050004457161214,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity gzsuga is
  port (xpiazcloha : in real; jn : in std_logic_vector(4 to 3); sjvluwut : linkage real);
end gzsuga;

architecture kvjqmdavy of gzsuga is
  
begin
  
end kvjqmdavy;

library ieee;
use ieee.std_logic_1164.all;

entity taricnq is
  port (vcemw : inout boolean_vector(0 downto 3); qrg : in time; pjb : linkage std_logic_vector(1 to 4));
end taricnq;

library ieee;
use ieee.std_logic_1164.all;

architecture xenurdyosd of taricnq is
  signal fkfbyhj : real;
  signal agcoehwy : real;
  signal nkrmfoh : std_logic_vector(4 to 3);
  signal lxyhqybigz : real;
begin
  rjtxqekxpd : entity work.gzsuga
    port map (xpiazcloha => lxyhqybigz, jn => nkrmfoh, sjvluwut => agcoehwy);
  abvlms : entity work.gzsuga
    port map (xpiazcloha => lxyhqybigz, jn => nkrmfoh, sjvluwut => fkfbyhj);
  
  -- Single-driven assignments
  vcemw <= (others => TRUE);
  lxyhqybigz <= 2_4.44431;
  
  -- Multi-driven assignments
  nkrmfoh <= (others => '0');
end xenurdyosd;

entity ypk is
  port (illuvvvf : out integer);
end ypk;

library ieee;
use ieee.std_logic_1164.all;

architecture gt of ypk is
  signal hpo : real;
  signal hngzfvphc : real;
  signal ijbjvrgrvl : std_logic_vector(4 to 3);
  signal vuwvhz : real;
begin
  ofcdufofk : entity work.gzsuga
    port map (xpiazcloha => vuwvhz, jn => ijbjvrgrvl, sjvluwut => hngzfvphc);
  ns : entity work.gzsuga
    port map (xpiazcloha => hpo, jn => ijbjvrgrvl, sjvluwut => vuwvhz);
  vvqzg : entity work.gzsuga
    port map (xpiazcloha => vuwvhz, jn => ijbjvrgrvl, sjvluwut => hpo);
  
  -- Single-driven assignments
  illuvvvf <= 16#6F#;
  
  -- Multi-driven assignments
  ijbjvrgrvl <= "";
  ijbjvrgrvl <= ijbjvrgrvl;
  ijbjvrgrvl <= ijbjvrgrvl;
end gt;



-- Seed after: 3575529139837432653,14641901754878719179
