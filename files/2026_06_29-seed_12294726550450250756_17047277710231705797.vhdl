-- Seed: 12294726550450250756,17047277710231705797

entity qu is
  port (pn : buffer time_vector(0 downto 0); tr : linkage real; mev : out bit_vector(1 downto 2));
end qu;

architecture vanfatqs of qu is
  
begin
  -- Single-driven assignments
  mev <= (others => '0');
  pn <= (others => 414 ns);
end vanfatqs;



-- Seed after: 14125614569580024702,17047277710231705797
