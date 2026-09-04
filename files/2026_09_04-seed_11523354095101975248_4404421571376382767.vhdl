-- Seed: 11523354095101975248,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity yjsanpkr is
  port (dbn : linkage std_logic; nnbpsetl : inout std_logic_vector(3 downto 3));
end yjsanpkr;

architecture mh of yjsanpkr is
  
begin
  -- Multi-driven assignments
  nnbpsetl <= nnbpsetl;
  nnbpsetl <= (others => 'H');
  nnbpsetl <= nnbpsetl;
end mh;

library ieee;
use ieee.std_logic_1164.all;

entity fv is
  port (zwtvqekzlo : linkage std_logic_vector(4 downto 0); r : in std_logic);
end fv;

library ieee;
use ieee.std_logic_1164.all;

architecture z of fv is
  signal ppaxn : std_logic_vector(3 downto 3);
  signal fi : std_logic;
  signal ovrcofyifp : std_logic;
  signal zdq : std_logic_vector(3 downto 3);
  signal gcraxdfwo : std_logic;
begin
  elwqgim : entity work.yjsanpkr
    port map (dbn => gcraxdfwo, nnbpsetl => zdq);
  ipund : entity work.yjsanpkr
    port map (dbn => r, nnbpsetl => zdq);
  msu : entity work.yjsanpkr
    port map (dbn => ovrcofyifp, nnbpsetl => zdq);
  bkjoptnjkf : entity work.yjsanpkr
    port map (dbn => fi, nnbpsetl => ppaxn);
  
  -- Multi-driven assignments
  ppaxn <= zdq;
  gcraxdfwo <= 'L';
  ovrcofyifp <= 'L';
end z;



-- Seed after: 9148665116244429150,4404421571376382767
