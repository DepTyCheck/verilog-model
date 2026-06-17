-- Seed: 10606476925073266173,10557070023141912087

entity yjlvzk is
  port (tk : in time_vector(2 downto 3));
end yjlvzk;

architecture ofu of yjlvzk is
  
begin
  
end ofu;

library ieee;
use ieee.std_logic_1164.all;

entity tmbpimt is
  port (iyrach : in integer; iyqg : in character; ued : buffer std_logic_vector(3 to 2); cyfetcu : in real);
end tmbpimt;

architecture jjzzlqc of tmbpimt is
  signal qwxxzw : time_vector(2 downto 3);
  signal qsvgxhu : time_vector(2 downto 3);
begin
  ocnfkm : entity work.yjlvzk
    port map (tk => qsvgxhu);
  gqurclqa : entity work.yjlvzk
    port map (tk => qwxxzw);
  xljccjlv : entity work.yjlvzk
    port map (tk => qsvgxhu);
  
  -- Single-driven assignments
  qsvgxhu <= (others => 0 ns);
  qwxxzw <= (others => 0 ns);
end jjzzlqc;

library ieee;
use ieee.std_logic_1164.all;

entity kfbun is
  port (l : in std_logic; hoglv : inout std_logic; aqyms : buffer real);
end kfbun;

library ieee;
use ieee.std_logic_1164.all;

architecture h of kfbun is
  signal r : time_vector(2 downto 3);
  signal iawnv : time_vector(2 downto 3);
  signal bnnviz : std_logic_vector(3 to 2);
  signal edjxzy : character;
  signal wdu : integer;
begin
  yesypcmtl : entity work.tmbpimt
    port map (iyrach => wdu, iyqg => edjxzy, ued => bnnviz, cyfetcu => aqyms);
  w : entity work.yjlvzk
    port map (tk => iawnv);
  oabqdxlcnh : entity work.yjlvzk
    port map (tk => r);
  
  -- Single-driven assignments
  iawnv <= (others => 0 ns);
  r <= (others => 0 ns);
  wdu <= 2#0_1_1#;
  aqyms <= 2_3.1_2_1_3;
  edjxzy <= 'v';
  
  -- Multi-driven assignments
  bnnviz <= (others => '0');
end h;

entity mlgkadc is
  port (qzu : out boolean);
end mlgkadc;

architecture tiwvord of mlgkadc is
  signal lrezxvhcc : time_vector(2 downto 3);
begin
  ql : entity work.yjlvzk
    port map (tk => lrezxvhcc);
end tiwvord;



-- Seed after: 6687969063374846703,10557070023141912087
