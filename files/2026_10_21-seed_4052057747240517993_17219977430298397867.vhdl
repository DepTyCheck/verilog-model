-- Seed: 4052057747240517993,17219977430298397867

library ieee;
use ieee.std_logic_1164.all;

entity eoul is
  port (gdnpkdovex : buffer real; ttkybeu : out time; j : in real; zxrixqpow : linkage std_logic);
end eoul;

architecture ydnnnvwig of eoul is
  
begin
  
end ydnnnvwig;

library ieee;
use ieee.std_logic_1164.all;

entity gemd is
  port (yktlvm : buffer time; kvowfbm : linkage std_logic_vector(2 downto 4));
end gemd;

library ieee;
use ieee.std_logic_1164.all;

architecture jlamthvu of gemd is
  signal iigy : time;
  signal b : std_logic;
  signal h : time;
  signal mnycul : real;
  signal sswnstqxqf : std_logic;
  signal wonlyze : real;
  signal kiotizc : time;
  signal lpaznbiw : real;
begin
  ztesc : entity work.eoul
    port map (gdnpkdovex => lpaznbiw, ttkybeu => kiotizc, j => wonlyze, zxrixqpow => sswnstqxqf);
  ce : entity work.eoul
    port map (gdnpkdovex => mnycul, ttkybeu => h, j => lpaznbiw, zxrixqpow => b);
  yderjqc : entity work.eoul
    port map (gdnpkdovex => wonlyze, ttkybeu => iigy, j => mnycul, zxrixqpow => b);
  
  -- Single-driven assignments
  yktlvm <= 4 min;
  
  -- Multi-driven assignments
  sswnstqxqf <= sswnstqxqf;
end jlamthvu;

entity iu is
  port (k : buffer time; lzuki : buffer time_vector(1 to 1));
end iu;

architecture ystuvl of iu is
  
begin
  -- Single-driven assignments
  k <= 16#0765.A# ms;
  lzuki <= lzuki;
end ystuvl;



-- Seed after: 3255805699777193429,17219977430298397867
