-- Seed: 18403630650927150803,4122021602305298647

library ieee;
use ieee.std_logic_1164.all;

entity ktc is
  port (bzasqaj : buffer std_logic);
end ktc;

architecture uzbtdyqo of ktc is
  
begin
  -- Multi-driven assignments
  bzasqaj <= bzasqaj;
  bzasqaj <= 'W';
end uzbtdyqo;

entity cbamoqdv is
  port (llozqqlgxt : in time; ybuyielmp : buffer real);
end cbamoqdv;

library ieee;
use ieee.std_logic_1164.all;

architecture ghsnnpi of cbamoqdv is
  signal eykjbs : std_logic;
begin
  dassvdhntz : entity work.ktc
    port map (bzasqaj => eykjbs);
  
  -- Single-driven assignments
  ybuyielmp <= 1.2_1_1_4_0;
end ghsnnpi;

entity xzfwfyea is
  port (lqosvf : in time);
end xzfwfyea;

library ieee;
use ieee.std_logic_1164.all;

architecture w of xzfwfyea is
  signal tbokwjr : real;
  signal pzvwdoqbtw : time;
  signal fnkaz : std_logic;
begin
  qffro : entity work.ktc
    port map (bzasqaj => fnkaz);
  kwt : entity work.cbamoqdv
    port map (llozqqlgxt => pzvwdoqbtw, ybuyielmp => tbokwjr);
  
  -- Single-driven assignments
  pzvwdoqbtw <= lqosvf;
  
  -- Multi-driven assignments
  fnkaz <= 'U';
  fnkaz <= 'Z';
end w;

entity lqti is
  port (owgktog : in real; bbosnc : linkage bit_vector(3 to 4); afpan : in character);
end lqti;

architecture vhzgl of lqti is
  signal f : time;
begin
  jblds : entity work.xzfwfyea
    port map (lqosvf => f);
  
  -- Single-driven assignments
  f <= 4_0_0 ps;
end vhzgl;



-- Seed after: 8569103875654560791,4122021602305298647
