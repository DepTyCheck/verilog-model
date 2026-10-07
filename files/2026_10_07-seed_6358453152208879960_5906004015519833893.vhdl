-- Seed: 6358453152208879960,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity nla is
  port ( befjquwws : buffer std_logic_vector(1 downto 2)
  ; nyikfkn : inout real
  ; jqgjffdj : out std_logic
  ; cbygwsqrr : buffer std_logic_vector(4 downto 3)
  );
end nla;

architecture jqewr of nla is
  
begin
  -- Single-driven assignments
  nyikfkn <= nyikfkn;
  
  -- Multi-driven assignments
  cbygwsqrr <= cbygwsqrr;
end jqewr;

entity p is
  port (dnhmmhk : inout integer; pycvtj : out integer_vector(3 downto 2); k : in integer);
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture cvdjtuzf of p is
  signal kqsxsezh : std_logic_vector(4 downto 3);
  signal uodkpxos : real;
  signal evdkl : std_logic_vector(4 downto 3);
  signal vra : std_logic;
  signal bddlr : real;
  signal luks : std_logic_vector(4 downto 3);
  signal zolfgnjgsf : std_logic;
  signal tud : real;
  signal tslvrlo : std_logic_vector(1 downto 2);
begin
  gstmnkx : entity work.nla
    port map (befjquwws => tslvrlo, nyikfkn => tud, jqgjffdj => zolfgnjgsf, cbygwsqrr => luks);
  fyem : entity work.nla
    port map (befjquwws => tslvrlo, nyikfkn => bddlr, jqgjffdj => vra, cbygwsqrr => evdkl);
  iqorpbhpb : entity work.nla
    port map (befjquwws => tslvrlo, nyikfkn => uodkpxos, jqgjffdj => zolfgnjgsf, cbygwsqrr => kqsxsezh);
  
  -- Single-driven assignments
  dnhmmhk <= 2#0_1_0_1_1#;
  pycvtj <= (413, 4_1_2);
  
  -- Multi-driven assignments
  tslvrlo <= "";
  zolfgnjgsf <= 'Z';
  vra <= 'W';
  evdkl <= ('-', 'L');
end cvdjtuzf;



-- Seed after: 13038925929209169274,5906004015519833893
