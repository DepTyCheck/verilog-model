-- Seed: 17960801451662135404,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity klbycomoby is
  port (dyrudbrk : out time; qbygassp : out std_logic; lbvpj : inout time; fygyyzf : buffer std_logic_vector(1 to 1));
end klbycomoby;

architecture cmrx of klbycomoby is
  
begin
  
end cmrx;

entity lncpmwupzy is
  port (a : in time; jblvojvw : inout time; ppifosca : inout real);
end lncpmwupzy;

library ieee;
use ieee.std_logic_1164.all;

architecture iih of lncpmwupzy is
  signal zf : std_logic;
  signal v : time;
  signal zljcxbu : std_logic_vector(1 to 1);
  signal gwivphl : time;
  signal xzvxdwjy : std_logic;
  signal pfjktgapst : time;
  signal aawzzlcxu : time;
  signal sjbkm : time;
  signal gwig : std_logic_vector(1 to 1);
  signal ccgrqb : time;
  signal pfiipu : std_logic;
  signal uy : time;
begin
  polrogrgsn : entity work.klbycomoby
    port map (dyrudbrk => uy, qbygassp => pfiipu, lbvpj => ccgrqb, fygyyzf => gwig);
  imosplrh : entity work.klbycomoby
    port map (dyrudbrk => sjbkm, qbygassp => pfiipu, lbvpj => aawzzlcxu, fygyyzf => gwig);
  psj : entity work.klbycomoby
    port map (dyrudbrk => pfjktgapst, qbygassp => xzvxdwjy, lbvpj => gwivphl, fygyyzf => zljcxbu);
  isafsaqe : entity work.klbycomoby
    port map (dyrudbrk => v, qbygassp => zf, lbvpj => jblvojvw, fygyyzf => gwig);
  
  -- Single-driven assignments
  ppifosca <= 2#0100.0_1_0_1_1#;
end iih;



-- Seed after: 2181565211188052803,5906004015519833893
