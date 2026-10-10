-- Seed: 9669723083206201134,511364357853360275

entity hspfmbzkr is
  port (istnlg : buffer bit);
end hspfmbzkr;

architecture uilmnu of hspfmbzkr is
  
begin
  -- Single-driven assignments
  istnlg <= '0';
end uilmnu;

entity hmfd is
  port (xgitgrgkjn : buffer boolean_vector(2 to 3));
end hmfd;

architecture jzghl of hmfd is
  signal guyyqm : bit;
  signal ycrcweglfv : bit;
begin
  y : entity work.hspfmbzkr
    port map (istnlg => ycrcweglfv);
  npnhh : entity work.hspfmbzkr
    port map (istnlg => guyyqm);
end jzghl;

library ieee;
use ieee.std_logic_1164.all;

entity hsp is
  port (rifvnxrcr : buffer std_logic; njok : out bit_vector(0 to 1); ibmejoktm : linkage real);
end hsp;

architecture qejlmw of hsp is
  signal lpt : boolean_vector(2 to 3);
begin
  phlapsq : entity work.hmfd
    port map (xgitgrgkjn => lpt);
  
  -- Single-driven assignments
  njok <= njok;
  
  -- Multi-driven assignments
  rifvnxrcr <= 'W';
  rifvnxrcr <= rifvnxrcr;
end qejlmw;



-- Seed after: 281996047961066509,511364357853360275
