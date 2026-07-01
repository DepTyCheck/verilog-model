-- Seed: 4470358250837071546,6882842853887419669

entity mereqq is
  port (zs : inout boolean_vector(4 downto 2); r : linkage boolean);
end mereqq;

architecture armtco of mereqq is
  
begin
  -- Single-driven assignments
  zs <= (FALSE, TRUE, TRUE);
end armtco;



-- Seed after: 13337418005297127682,6882842853887419669
