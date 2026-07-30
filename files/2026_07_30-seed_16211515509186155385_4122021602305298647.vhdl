-- Seed: 16211515509186155385,4122021602305298647

entity cimjbf is
  port (td : inout boolean_vector(4 downto 0); id : in time);
end cimjbf;

architecture irceap of cimjbf is
  
begin
  -- Single-driven assignments
  td <= (FALSE, TRUE, TRUE, FALSE, FALSE);
end irceap;



-- Seed after: 15197417465736569232,4122021602305298647
