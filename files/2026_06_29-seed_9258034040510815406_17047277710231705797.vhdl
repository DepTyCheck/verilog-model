-- Seed: 9258034040510815406,17047277710231705797

entity qqgc is
  port (olt : buffer time; dytgjb : out real_vector(3 to 1); bzjtng : inout bit_vector(3 to 0));
end qqgc;

architecture ds of qqgc is
  
begin
  -- Single-driven assignments
  bzjtng <= (others => '0');
  olt <= 3_1_1.4_2 fs;
  dytgjb <= (others => 0.0);
end ds;



-- Seed after: 12651431119388761767,17047277710231705797
