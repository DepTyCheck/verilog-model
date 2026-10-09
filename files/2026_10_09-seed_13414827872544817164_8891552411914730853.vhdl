-- Seed: 13414827872544817164,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity cpbayyjnxp is
  port (fjjpblvfs : inout time; wnxdymxnzr : in std_logic);
end cpbayyjnxp;

architecture el of cpbayyjnxp is
  
begin
  
end el;

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (bxcjtkwzr : buffer character; rkwfn : out std_logic_vector(1 to 4); uspqoq : buffer time);
end m;

library ieee;
use ieee.std_logic_1164.all;

architecture hmdlpu of m is
  signal fjfwefvnf : std_logic;
  signal jhqga : time;
  signal vhxabfiap : std_logic;
  signal xccwlscs : time;
begin
  qlxpfibpwf : entity work.cpbayyjnxp
    port map (fjjpblvfs => xccwlscs, wnxdymxnzr => vhxabfiap);
  bsahdovafe : entity work.cpbayyjnxp
    port map (fjjpblvfs => jhqga, wnxdymxnzr => fjfwefvnf);
  
  -- Single-driven assignments
  uspqoq <= jhqga;
  
  -- Multi-driven assignments
  rkwfn <= ('W', 'X', '-', '0');
  fjfwefvnf <= 'Z';
  vhxabfiap <= 'U';
end hmdlpu;



-- Seed after: 8101312101412560533,8891552411914730853
