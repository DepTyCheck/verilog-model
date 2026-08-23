-- Seed: 9718680258875189425,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity sgscr is
  port (ko : buffer std_logic);
end sgscr;

architecture mvzd of sgscr is
  
begin
  -- Multi-driven assignments
  ko <= '-';
end mvzd;

entity ac is
  port (dsrgyq : inout time; wzhc : in time);
end ac;

library ieee;
use ieee.std_logic_1164.all;

architecture samp of ac is
  signal h : std_logic;
  signal slplftkk : std_logic;
  signal wqgbhxcl : std_logic;
begin
  fiejccy : entity work.sgscr
    port map (ko => wqgbhxcl);
  nfoadwa : entity work.sgscr
    port map (ko => slplftkk);
  oc : entity work.sgscr
    port map (ko => h);
  
  -- Single-driven assignments
  dsrgyq <= 8#70.6_1_4# ps;
  
  -- Multi-driven assignments
  h <= h;
  h <= wqgbhxcl;
  h <= slplftkk;
  wqgbhxcl <= wqgbhxcl;
end samp;

entity kgrzw is
  port (hrx : linkage real; njufaitl : out real);
end kgrzw;

library ieee;
use ieee.std_logic_1164.all;

architecture bcpabqnhpm of kgrzw is
  signal zcunpl : std_logic;
  signal btsstxfwah : time;
begin
  tami : entity work.ac
    port map (dsrgyq => btsstxfwah, wzhc => btsstxfwah);
  rxslqt : entity work.sgscr
    port map (ko => zcunpl);
  
  -- Single-driven assignments
  njufaitl <= 2#1_1.00#;
  
  -- Multi-driven assignments
  zcunpl <= 'L';
end bcpabqnhpm;



-- Seed after: 14696492275448073004,4245627776430562977
