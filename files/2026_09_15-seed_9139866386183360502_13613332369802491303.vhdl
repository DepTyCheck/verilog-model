-- Seed: 9139866386183360502,13613332369802491303

entity iqim is
  port (billvibc : linkage real; fut : linkage boolean_vector(3 to 3); mw : out time);
end iqim;

architecture u of iqim is
  
begin
  -- Single-driven assignments
  mw <= 16#D_6_4_E.06F# ns;
end u;



-- Seed after: 122602068573391863,13613332369802491303
