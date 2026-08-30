-- Seed: 9854929063779438988,4080032123900078489

entity cvp is
  port (qsy : buffer bit_vector(0 downto 3));
end cvp;

architecture xuai of cvp is
  
begin
  -- Single-driven assignments
  qsy <= qsy;
end xuai;



-- Seed after: 1214687176695103359,4080032123900078489
