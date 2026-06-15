-- Seed: 350671493725251583,15300320181035395489

entity wloixznw is
  port (air : in real; jzqff : inout real; cv : in time_vector(3 downto 2));
end wloixznw;

architecture ndq of wloixznw is
  
begin
  -- Single-driven assignments
  jzqff <= 2#00.0_1#;
end ndq;

library ieee;
use ieee.std_logic_1164.all;

entity pinfrf is
  port (cadkkz : out std_logic; zaxyate : linkage real);
end pinfrf;

architecture xlimqmm of pinfrf is
  signal x : real;
  signal bnlfeemnjw : time_vector(3 downto 2);
  signal ctvcittm : real;
  signal jvcugic : time_vector(3 downto 2);
  signal u : real;
  signal ke : real;
begin
  qiaxy : entity work.wloixznw
    port map (air => ke, jzqff => u, cv => jvcugic);
  bgnmt : entity work.wloixznw
    port map (air => ctvcittm, jzqff => ke, cv => bnlfeemnjw);
  hi : entity work.wloixznw
    port map (air => x, jzqff => ctvcittm, cv => jvcugic);
  ewusevsvg : entity work.wloixznw
    port map (air => ke, jzqff => x, cv => jvcugic);
end xlimqmm;

library ieee;
use ieee.std_logic_1164.all;

entity jmhjxwuufl is
  port (ouewv : out std_logic_vector(3 to 1); xgu : inout real_vector(1 to 2); lihcf : buffer integer);
end jmhjxwuufl;

architecture sgy of jmhjxwuufl is
  signal ye : real;
  signal exf : time_vector(3 downto 2);
  signal pzbv : real;
  signal lwrzhr : real;
begin
  pn : entity work.wloixznw
    port map (air => lwrzhr, jzqff => pzbv, cv => exf);
  poujk : entity work.wloixznw
    port map (air => ye, jzqff => ye, cv => exf);
  
  -- Multi-driven assignments
  ouewv <= (others => '0');
  ouewv <= (others => '0');
end sgy;



-- Seed after: 46465586649330571,15300320181035395489
