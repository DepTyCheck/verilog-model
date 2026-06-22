-- Seed: 1534932415372619450,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity zeduvtjum is
  port (v : linkage std_logic);
end zeduvtjum;

architecture sriqkg of zeduvtjum is
  
begin
  
end sriqkg;

entity j is
  port (zqqig : in integer);
end j;

architecture lhuqmltl of j is
  
begin
  
end lhuqmltl;

entity niu is
  port (ocu : in time);
end niu;

library ieee;
use ieee.std_logic_1164.all;

architecture ijtkfdnav of niu is
  signal uoyzoir : std_logic;
begin
  xg : entity work.zeduvtjum
    port map (v => uoyzoir);
end ijtkfdnav;

library ieee;
use ieee.std_logic_1164.all;

entity xhf is
  port (afvvqnmzt : in std_logic; rduzx : in std_logic; rpsc : buffer time; nrypyw : inout boolean_vector(4 downto 1));
end xhf;

architecture yjgnrrx of xhf is
  signal qrjrnqh : time;
  signal gchnkwp : integer;
begin
  oqrjqvm : entity work.j
    port map (zqqig => gchnkwp);
  fmqz : entity work.zeduvtjum
    port map (v => afvvqnmzt);
  xjhjp : entity work.niu
    port map (ocu => qrjrnqh);
  ovhol : entity work.zeduvtjum
    port map (v => rduzx);
  
  -- Single-driven assignments
  qrjrnqh <= 2#0_0_1_1# ms;
  rpsc <= 0_4.4_3_3_3_2 ms;
  nrypyw <= (TRUE, FALSE, FALSE, FALSE);
end yjgnrrx;



-- Seed after: 12198969635251667644,13479070923501788437
