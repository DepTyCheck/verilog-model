-- Seed: 7253986469035754770,12011142928354116943

entity hvd is
  port (ploigxzrfd : linkage time; sulik : in boolean_vector(2 to 2));
end hvd;

architecture gzmymof of hvd is
  
begin
  
end gzmymof;

entity msntn is
  port (mmytky : out integer_vector(1 downto 2));
end msntn;

architecture xulhemuxki of msntn is
  signal hcuspycij : time;
  signal tjof : boolean_vector(2 to 2);
  signal ctguy : time;
  signal sa : boolean_vector(2 to 2);
  signal dibjv : time;
begin
  ldcyxoi : entity work.hvd
    port map (ploigxzrfd => dibjv, sulik => sa);
  rlb : entity work.hvd
    port map (ploigxzrfd => ctguy, sulik => tjof);
  pvralhmefi : entity work.hvd
    port map (ploigxzrfd => hcuspycij, sulik => sa);
  
  -- Single-driven assignments
  mmytky <= (others => 0);
  tjof <= (others => FALSE);
  sa <= (others => FALSE);
end xulhemuxki;

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (mkmofmu : out std_logic_vector(3 downto 3); csrd : in real_vector(2 downto 4));
end y;

architecture inor of y is
  signal vsqy : boolean_vector(2 to 2);
  signal ehjgulqqt : time;
begin
  yqwcg : entity work.hvd
    port map (ploigxzrfd => ehjgulqqt, sulik => vsqy);
  
  -- Single-driven assignments
  vsqy <= (others => TRUE);
  
  -- Multi-driven assignments
  mkmofmu <= "H";
  mkmofmu <= (others => 'L');
end inor;



-- Seed after: 7102406833080009737,12011142928354116943
