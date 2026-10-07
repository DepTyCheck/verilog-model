-- Seed: 7522875131355008323,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity oymrud is
  port (uhmunfk : linkage std_logic; a : inout integer);
end oymrud;

architecture cgvhwsuy of oymrud is
  
begin
  -- Single-driven assignments
  a <= 2#0#;
end cgvhwsuy;

entity wvms is
  port (brdrynei : buffer integer_vector(4 to 1); lcitqi : out integer);
end wvms;

architecture yll of wvms is
  
begin
  -- Single-driven assignments
  lcitqi <= 2#1_1_1_0_1#;
  brdrynei <= brdrynei;
end yll;

entity qdmdihlxjm is
  port (iltxyjthm : buffer bit);
end qdmdihlxjm;

architecture r of qdmdihlxjm is
  signal swsckijogx : integer;
  signal mnujkvkym : integer_vector(4 to 1);
begin
  fzatm : entity work.wvms
    port map (brdrynei => mnujkvkym, lcitqi => swsckijogx);
  
  -- Single-driven assignments
  iltxyjthm <= iltxyjthm;
end r;

entity wp is
  port (xugayxbt : inout bit);
end wp;

library ieee;
use ieee.std_logic_1164.all;

architecture iqdmpjbz of wp is
  signal kxtrc : integer;
  signal ge : std_logic;
  signal vgczn : integer;
  signal tfvui : std_logic;
  signal kmf : integer;
  signal s : std_logic;
begin
  mdikxdypy : entity work.qdmdihlxjm
    port map (iltxyjthm => xugayxbt);
  zdq : entity work.oymrud
    port map (uhmunfk => s, a => kmf);
  wzoibdzk : entity work.oymrud
    port map (uhmunfk => tfvui, a => vgczn);
  ljp : entity work.oymrud
    port map (uhmunfk => ge, a => kxtrc);
  
  -- Multi-driven assignments
  tfvui <= 'W';
  s <= '-';
end iqdmpjbz;



-- Seed after: 16215936620704885992,5906004015519833893
