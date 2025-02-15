///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn Pool(comptime T: type, comptime max_index: usize) type {
    return struct {
        storage: [max_index]T = undefined,
        used: [max_index]bool = .{false} ** max_index,
        next_index: usize = 0,

        const Self = @This();

        pub fn create(self: *Self) ?usize {
            for (0..max_index) |_| {
                if (self.used[self.next_index] == false) {
                    const index = self.next_index;
                    self.used[index] = true;
                    self.storage[index] = undefined;
                    self.next_index = (self.next_index + 1) % max_index;
                    return index;
                }
                self.next_index = (self.next_index + 1) % max_index;
            }
            return null;
        }

        pub fn get(self: *Self, index: usize) ?*T {
            if (index >= max_index) return null;
            if (self.used[index] == false) return null;

            return &self.storage[index];
        }

        pub fn delete(self: *Self, index: usize) void {
            if (index >= max_index) return;
            if (self.used[index] == false) return;

            self.used[index] = false;
            self.storage[index] = undefined;
        }
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
