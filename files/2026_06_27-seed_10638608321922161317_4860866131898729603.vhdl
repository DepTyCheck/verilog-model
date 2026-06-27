-- Seed: 10638608321922161317,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity gkc is
  port (dtrjmbcsn : inout std_logic; o : in real; nsn : in severity_level);
end gkc;

architecture fnls of gkc is
  
begin
  -- Multi-driven assignments
  dtrjmbcsn <= '-';
  dtrjmbcsn <= 'X';
  dtrjmbcsn <= 'L';
end fnls;

entity cxpt is
  port (scqhlp : out real);
end cxpt;

library ieee;
use ieee.std_logic_1164.all;

architecture mqsh of cxpt is
  signal qaqijq : severity_level;
  signal r : real;
  signal nqvf : real;
  signal thwphvb : severity_level;
  signal vibzix : real;
  signal czgglabvp : std_logic;
begin
  faacpywtcb : entity work.gkc
    port map (dtrjmbcsn => czgglabvp, o => vibzix, nsn => thwphvb);
  qvhg : entity work.gkc
    port map (dtrjmbcsn => czgglabvp, o => nqvf, nsn => thwphvb);
  fsjosjlmp : entity work.gkc
    port map (dtrjmbcsn => czgglabvp, o => r, nsn => qaqijq);
  
  -- Single-driven assignments
  scqhlp <= 2_0_1_3.03223;
  qaqijq <= WARNING;
  
  -- Multi-driven assignments
  czgglabvp <= 'L';
  czgglabvp <= '-';
  czgglabvp <= 'H';
  czgglabvp <= 'L';
end mqsh;

library ieee;
use ieee.std_logic_1164.all;

entity mswiukea is
  port (zoutabqrks : linkage time; hmbfwg : in std_logic);
end mswiukea;

architecture s of mswiukea is
  signal hytu : real;
  signal tcc : real;
begin
  dcsdk : entity work.cxpt
    port map (scqhlp => tcc);
  zrgkjt : entity work.cxpt
    port map (scqhlp => hytu);
end s;

library ieee;
use ieee.std_logic_1164.all;

entity xukup is
  port ( wjrrri : buffer std_logic_vector(0 downto 3)
  ; eov : linkage real
  ; uiho : buffer std_logic_vector(0 to 3)
  ; xadgiqsqtk : linkage bit_vector(0 downto 2)
  );
end xukup;

library ieee;
use ieee.std_logic_1164.all;

architecture jgectzw of xukup is
  signal deaka : std_logic;
  signal nvnlkqpnas : time;
  signal mdsouvw : std_logic;
  signal tybq : severity_level;
  signal zghmbs : std_logic;
  signal lvy : severity_level;
  signal rrdnjw : real;
  signal bcd : std_logic;
begin
  mqpzji : entity work.gkc
    port map (dtrjmbcsn => bcd, o => rrdnjw, nsn => lvy);
  bnidza : entity work.gkc
    port map (dtrjmbcsn => zghmbs, o => rrdnjw, nsn => tybq);
  ntsvnuiu : entity work.gkc
    port map (dtrjmbcsn => mdsouvw, o => rrdnjw, nsn => lvy);
  uooaaj : entity work.mswiukea
    port map (zoutabqrks => nvnlkqpnas, hmbfwg => deaka);
  
  -- Single-driven assignments
  rrdnjw <= 16#A_7_9_2.3#;
  lvy <= ERROR;
end jgectzw;



-- Seed after: 14553287578176594536,4860866131898729603
