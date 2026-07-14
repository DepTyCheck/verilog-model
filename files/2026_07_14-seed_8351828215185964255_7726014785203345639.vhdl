-- Seed: 8351828215185964255,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity tbvvshrbu is
  port (biazbxr : in std_logic; otg : inout access_value_mirror; mpwtgrsekc : buffer std_logic_vector(2 downto 2));
end tbvvshrbu;

architecture hqchbaxf of tbvvshrbu is
  
begin
  
end hqchbaxf;

library ieee;
use ieee.std_logic_1164.all;

entity hqvhxmegc is
  port (ycautbxds : linkage time; jsifhlnyv : out std_logic_vector(3 to 1));
end hqvhxmegc;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture fzergfoncy of hqvhxmegc is
  signal exu : std_logic_vector(2 downto 2);
  shared variable dmitn : access_value_mirror;
  signal ti : std_logic_vector(2 downto 2);
  shared variable haka : access_value_mirror;
  signal tlv : std_logic_vector(2 downto 2);
  shared variable xsohhu : access_value_mirror;
  signal vao : std_logic;
begin
  nfbaffsc : entity work.tbvvshrbu
    port map (biazbxr => vao, otg => xsohhu, mpwtgrsekc => tlv);
  arppnupd : entity work.tbvvshrbu
    port map (biazbxr => vao, otg => haka, mpwtgrsekc => ti);
  p : entity work.tbvvshrbu
    port map (biazbxr => vao, otg => dmitn, mpwtgrsekc => exu);
  
  -- Multi-driven assignments
  jsifhlnyv <= "";
  jsifhlnyv <= "";
  vao <= vao;
end fzergfoncy;



-- Seed after: 14061375308046152876,7726014785203345639
