-- Seed: 2661257103428069023,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (jacfrc : inout time; fotrix : inout boolean_vector(3 to 3); mbqj : out bit; fb : inout std_logic_vector(4 to 4));
end g;

architecture rjk of g is
  
begin
  -- Multi-driven assignments
  fb <= (others => 'X');
end rjk;

library ieee;
use ieee.std_logic_1164.all;

entity hbmk is
  port (aroph : linkage std_logic; n : inout std_logic_vector(0 to 2));
end hbmk;

library ieee;
use ieee.std_logic_1164.all;

architecture dg of hbmk is
  signal ikvsosxp : std_logic_vector(4 to 4);
  signal rqpnvyiqo : bit;
  signal vevhzfb : boolean_vector(3 to 3);
  signal bqajw : time;
  signal zpmoghbs : std_logic_vector(4 to 4);
  signal njlpfafan : bit;
  signal hd : boolean_vector(3 to 3);
  signal m : time;
begin
  zzxwswn : entity work.g
    port map (jacfrc => m, fotrix => hd, mbqj => njlpfafan, fb => zpmoghbs);
  mtzxvrhbnw : entity work.g
    port map (jacfrc => bqajw, fotrix => vevhzfb, mbqj => rqpnvyiqo, fb => ikvsosxp);
  
  -- Multi-driven assignments
  n <= ('-', 'H', 'W');
end dg;

entity bkcxvuz is
  port (k : out integer);
end bkcxvuz;

library ieee;
use ieee.std_logic_1164.all;

architecture fveknyaj of bkcxvuz is
  signal wkzwwiibv : bit;
  signal mfrq : boolean_vector(3 to 3);
  signal vew : time;
  signal dszgcohptm : std_logic_vector(4 to 4);
  signal zboih : bit;
  signal krclxgusm : boolean_vector(3 to 3);
  signal gtsiym : time;
  signal tcltabxb : bit;
  signal gkrcw : boolean_vector(3 to 3);
  signal aicjil : time;
  signal csztcdakur : std_logic_vector(4 to 4);
  signal qs : bit;
  signal pyh : boolean_vector(3 to 3);
  signal ksuvzerg : time;
begin
  cfkrt : entity work.g
    port map (jacfrc => ksuvzerg, fotrix => pyh, mbqj => qs, fb => csztcdakur);
  gsw : entity work.g
    port map (jacfrc => aicjil, fotrix => gkrcw, mbqj => tcltabxb, fb => csztcdakur);
  d : entity work.g
    port map (jacfrc => gtsiym, fotrix => krclxgusm, mbqj => zboih, fb => dszgcohptm);
  rfmn : entity work.g
    port map (jacfrc => vew, fotrix => mfrq, mbqj => wkzwwiibv, fb => csztcdakur);
  
  -- Single-driven assignments
  k <= 16#7_B_B_C#;
  
  -- Multi-driven assignments
  csztcdakur <= (others => '0');
  csztcdakur <= (others => 'U');
  csztcdakur <= "U";
  csztcdakur <= "-";
end fveknyaj;



-- Seed after: 13766144358101926448,3108530264173481209
