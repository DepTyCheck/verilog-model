-- Seed: 6093751292418228350,7311216359267151659

entity newkhm is
  port (ogmnnd : out time);
end newkhm;

architecture q of newkhm is
  
begin
  -- Single-driven assignments
  ogmnnd <= 16#8_3_4.4_9_E_D# ns;
end q;



-- Seed after: 10389554937083599260,7311216359267151659
