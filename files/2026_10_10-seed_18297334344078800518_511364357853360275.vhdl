-- Seed: 18297334344078800518,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity edtov is
  port (ucvyziqbei : out time; qyqli : buffer std_logic; no : out real);
end edtov;

architecture mg of edtov is
  
begin
  -- Single-driven assignments
  no <= 2#1_1_1_0.1_1_0#;
  ucvyziqbei <= 1033 ms;
  
  -- Multi-driven assignments
  qyqli <= qyqli;
  qyqli <= qyqli;
  qyqli <= 'H';
end mg;

entity tl is
  port (uy : out integer; aon : buffer real; g : in boolean_vector(0 to 0));
end tl;

library ieee;
use ieee.std_logic_1164.all;

architecture hao of tl is
  signal btmiuicub : real;
  signal xxn : std_logic;
  signal x : time;
  signal ajjvrcoxh : time;
  signal kgnbegktm : real;
  signal eyowxd : time;
  signal okkyn : real;
  signal gqdyjxxkee : std_logic;
  signal idx : time;
begin
  opy : entity work.edtov
    port map (ucvyziqbei => idx, qyqli => gqdyjxxkee, no => okkyn);
  m : entity work.edtov
    port map (ucvyziqbei => eyowxd, qyqli => gqdyjxxkee, no => kgnbegktm);
  vkfwbva : entity work.edtov
    port map (ucvyziqbei => ajjvrcoxh, qyqli => gqdyjxxkee, no => aon);
  wgfjqtl : entity work.edtov
    port map (ucvyziqbei => x, qyqli => xxn, no => btmiuicub);
  
  -- Single-driven assignments
  uy <= 8#3#;
  
  -- Multi-driven assignments
  xxn <= gqdyjxxkee;
  gqdyjxxkee <= gqdyjxxkee;
  gqdyjxxkee <= 'W';
end hao;



-- Seed after: 144299006450881378,511364357853360275
