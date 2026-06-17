-- Seed: 13236093929952280752,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity sylfqhxl is
  port (itcg : linkage std_logic_vector(3 to 0); zesvzx : in time; ygopghuw : in real; jkrboixn : buffer character);
end sylfqhxl;

architecture hrvs of sylfqhxl is
  
begin
  -- Single-driven assignments
  jkrboixn <= 'p';
end hrvs;

entity nbh is
  port (ge : in real);
end nbh;

library ieee;
use ieee.std_logic_1164.all;

architecture msr of nbh is
  signal czpq : character;
  signal azyy : time;
  signal pxou : std_logic_vector(3 to 0);
begin
  ufzpapq : entity work.sylfqhxl
    port map (itcg => pxou, zesvzx => azyy, ygopghuw => ge, jkrboixn => czpq);
  
  -- Multi-driven assignments
  pxou <= (others => '0');
  pxou <= (others => '0');
end msr;

entity mseefew is
  port (ycfq : in real; xpfv : in boolean_vector(4 downto 3); a : linkage string(2 downto 3); ea : in time);
end mseefew;

library ieee;
use ieee.std_logic_1164.all;

architecture leqhugn of mseefew is
  signal rgiokiw : character;
  signal bnpptjqsw : real;
  signal qyy : character;
  signal kxftf : real;
  signal ipe : time;
  signal xpcvc : std_logic_vector(3 to 0);
  signal vke : character;
  signal rdywdet : time;
  signal ny : std_logic_vector(3 to 0);
begin
  terzzgom : entity work.sylfqhxl
    port map (itcg => ny, zesvzx => rdywdet, ygopghuw => ycfq, jkrboixn => vke);
  woooypu : entity work.sylfqhxl
    port map (itcg => xpcvc, zesvzx => ipe, ygopghuw => kxftf, jkrboixn => qyy);
  uc : entity work.nbh
    port map (ge => ycfq);
  xtpbqbg : entity work.sylfqhxl
    port map (itcg => ny, zesvzx => ipe, ygopghuw => bnpptjqsw, jkrboixn => rgiokiw);
  
  -- Single-driven assignments
  bnpptjqsw <= 2#0.1_1_1_0_1#;
  kxftf <= 16#D.6BC2#;
  rdywdet <= 2 sec;
  ipe <= 4 min;
  
  -- Multi-driven assignments
  xpcvc <= (others => '0');
  ny <= (others => '0');
  xpcvc <= (others => '0');
  ny <= "";
end leqhugn;



-- Seed after: 5187509822662272839,10557070023141912087
