-- Seed: 8020528095591871426,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity kymmzh is
  port (lhxis : buffer real; zvwmk : in std_logic_vector(1 downto 4));
end kymmzh;

architecture kxs of kymmzh is
  
begin
  
end kxs;

entity ailj is
  port (xk : inout integer_vector(2 downto 0));
end ailj;

library ieee;
use ieee.std_logic_1164.all;

architecture v of ailj is
  signal jcti : std_logic_vector(1 downto 4);
  signal cnzurjiskt : real;
  signal gts : real;
  signal egcgfj : std_logic_vector(1 downto 4);
  signal c : real;
  signal zudjwbuoq : std_logic_vector(1 downto 4);
  signal om : real;
begin
  bntorroq : entity work.kymmzh
    port map (lhxis => om, zvwmk => zudjwbuoq);
  khfshbp : entity work.kymmzh
    port map (lhxis => c, zvwmk => egcgfj);
  nk : entity work.kymmzh
    port map (lhxis => gts, zvwmk => zudjwbuoq);
  biehk : entity work.kymmzh
    port map (lhxis => cnzurjiskt, zvwmk => jcti);
  
  -- Single-driven assignments
  xk <= (2#01000#, 23, 1141);
  
  -- Multi-driven assignments
  zudjwbuoq <= (others => '0');
  egcgfj <= (others => '0');
end v;



-- Seed after: 1293077442493123028,4404421571376382767
