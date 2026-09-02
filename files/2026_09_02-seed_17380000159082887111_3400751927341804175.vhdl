-- Seed: 17380000159082887111,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity ti is
  port (hnshtgyb : out std_logic_vector(3 to 0));
end ti;

architecture gk of ti is
  
begin
  -- Multi-driven assignments
  hnshtgyb <= hnshtgyb;
  hnshtgyb <= hnshtgyb;
  hnshtgyb <= hnshtgyb;
  hnshtgyb <= "";
end gk;

library ieee;
use ieee.std_logic_1164.all;

entity jcb is
  port (jbka : out real; hrs : out std_logic_vector(0 downto 4));
end jcb;

architecture hieqogfbp of jcb is
  
begin
  tiibkpr : entity work.ti
    port map (hnshtgyb => hrs);
  kojzlyymem : entity work.ti
    port map (hnshtgyb => hrs);
  
  -- Single-driven assignments
  jbka <= jbka;
  
  -- Multi-driven assignments
  hrs <= (others => '0');
end hieqogfbp;

entity hkalbmwh is
  port (q : linkage boolean);
end hkalbmwh;

library ieee;
use ieee.std_logic_1164.all;

architecture fcbfkzm of hkalbmwh is
  signal shzf : real;
  signal zkssohpf : std_logic_vector(0 downto 4);
begin
  glkb : entity work.ti
    port map (hnshtgyb => zkssohpf);
  fpjmyo : entity work.jcb
    port map (jbka => shzf, hrs => zkssohpf);
end fcbfkzm;

entity nruqz is
  port (qjy : inout real; smycimri : in real);
end nruqz;

library ieee;
use ieee.std_logic_1164.all;

architecture tfejbfozdr of nruqz is
  signal hdbvzt : std_logic_vector(3 to 0);
  signal e : std_logic_vector(3 to 0);
begin
  doiguov : entity work.ti
    port map (hnshtgyb => e);
  tmmrjjsz : entity work.ti
    port map (hnshtgyb => hdbvzt);
  
  -- Single-driven assignments
  qjy <= 8#531.1410#;
  
  -- Multi-driven assignments
  e <= e;
  e <= e;
  e <= e;
  e <= e;
end tfejbfozdr;



-- Seed after: 1081577635530486200,3400751927341804175
