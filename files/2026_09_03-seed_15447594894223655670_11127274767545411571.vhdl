-- Seed: 15447594894223655670,11127274767545411571

entity utgogz is
  port (qrity : inout character);
end utgogz;

architecture zpufzjh of utgogz is
  
begin
  -- Single-driven assignments
  qrity <= 'l';
end zpufzjh;

entity tnbblw is
  port (nxifrlhvlp : inout real);
end tnbblw;

architecture fsjgzgkjah of tnbblw is
  
begin
  -- Single-driven assignments
  nxifrlhvlp <= 10.44033;
end fsjgzgkjah;

library ieee;
use ieee.std_logic_1164.all;

entity rbhqtyej is
  port ( pnjwutct : out severity_level
  ; uorrrfvfc : buffer std_logic_vector(4 to 0)
  ; mignetj : buffer boolean_vector(2 downto 2)
  ; tzex : inout time_vector(3 downto 4)
  );
end rbhqtyej;

architecture g of rbhqtyej is
  signal mhw : real;
  signal ttfguk : character;
begin
  ghzmaq : entity work.utgogz
    port map (qrity => ttfguk);
  wtqlkx : entity work.tnbblw
    port map (nxifrlhvlp => mhw);
  
  -- Single-driven assignments
  tzex <= (others => 0 ns);
  mignetj <= (others => FALSE);
  
  -- Multi-driven assignments
  uorrrfvfc <= uorrrfvfc;
  uorrrfvfc <= uorrrfvfc;
  uorrrfvfc <= uorrrfvfc;
end g;

library ieee;
use ieee.std_logic_1164.all;

entity tbrgjsp is
  port (oe : buffer std_logic_vector(0 downto 2));
end tbrgjsp;

architecture k of tbrgjsp is
  signal klbx : time_vector(3 downto 4);
  signal wgigybuq : boolean_vector(2 downto 2);
  signal tjvkecvuk : severity_level;
  signal vov : character;
  signal p : real;
begin
  d : entity work.tnbblw
    port map (nxifrlhvlp => p);
  fhskrml : entity work.utgogz
    port map (qrity => vov);
  vhqoc : entity work.rbhqtyej
    port map (pnjwutct => tjvkecvuk, uorrrfvfc => oe, mignetj => wgigybuq, tzex => klbx);
  
  -- Multi-driven assignments
  oe <= oe;
end k;



-- Seed after: 3363361421108246785,11127274767545411571
