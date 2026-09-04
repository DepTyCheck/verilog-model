-- Seed: 4936606353951785538,4404421571376382767

entity byblp is
  port (hku : out time; vkhk : buffer time);
end byblp;

architecture ope of byblp is
  
begin
  -- Single-driven assignments
  vkhk <= vkhk;
  hku <= 4.21 ps;
end ope;



-- Seed after: 12244813045743184453,4404421571376382767
