const std = @import("std");
const Allocator = std.mem.Allocator;
const SinglyLinkedList = std.SinglyLinkedList;

pub fn BucketArrayUnmanaged(comptime N: usize, comptime T: type) type {
    return struct {
        buckets: List = List{ .first = null },
        write_index: usize = 0,
        len: usize = 0,

        const This = @This();
        const List = SinglyLinkedList(Bucket);
        const Bucket = [N]T;

        pub fn init() This {
            return This{};
        }

        pub fn deinit(this: *This, allocator: Allocator) void {
            var it = this.buckets.first;
            while (it) |bucket| {
                allocator.destroy(bucket);
                it = bucket.next;
            }
        }

        fn makeNode(allocator: Allocator) !*List.Node {
            var node = try allocator.create(List.Node);
            node.next = null;
            return node;
        }

        pub fn push(this: *This, allocator: Allocator, item: T) !void {
            if (this.buckets.first == null) {
                this.buckets.first = try makeNode(allocator);
            } else if (this.write_index == N) {
                const node = try makeNode(allocator);
                this.buckets.prepend(node);
                this.write_index = 0;
            }

            const ptr = &this.buckets.first.?.data[this.write_index];
            ptr.* = item;

            this.write_index += 1;
            this.len += 1;
        }

        pub fn pop(this: *This, allocator: Allocator) void {
            if (this.write_index == 0) {
                const node = this.buckets.popFirst() orelse return;
                this.buckets.first = node.next;
                this.write_index = N;
                allocator.destroy(node);
            } else {
                this.write_index -= 1;
            }

            this.len -= 1;
        }

        pub fn top(this: *This) ?*T {
            if (this.write_index == 0) {
                const node = this.buckets.first orelse return null;
                const next = node.next orelse return null;
                return &next.data[0];
            } else {
                const node = this.buckets.first orelse return null;
                return &node.data[this.write_index - 1];
            }
        }

        pub fn iterator(this: *This) Iterator(N, T) {
            return Iterator(N, T).init(this.buckets.first, this.write_index);
        }
    };
}

pub fn BucketArray(comptime N: usize, comptime T: type) type {
    return struct {
        allocator: Allocator,
        inner: BucketArrayUnmanaged(N, T),

        const This = @This();
        const List = BucketArrayUnmanaged(N, T).List;
        const Bucket = BucketArrayUnmanaged(N, T).Bucket;

        pub fn init(allocator: Allocator) This {
            return This{
                .allocator = allocator,
                .inner = BucketArrayUnmanaged(N, T).init(),
            };
        }

        pub fn deinit(this: *This) void {
            this.inner.deinit(this.allocator);
        }

        pub fn push(this: *This, item: T) !void {
            return this.inner.push(this.allocator, item);
        }

        pub fn pop(this: *This) void {
            return this.inner.pop(this.allocator);
        }

        pub fn top(this: *This) ?*T {
            return this.inner.top();
        }

        pub fn iterator(this: *This) Iterator(N, T) {
            return this.inner.iterator();
        }
    };
}

pub fn Iterator(comptime N: usize, comptime T: type) type {
    return struct {
        current_bucket: ?*BucketArrayUnmanaged(N, T).List.Node,
        current_index: usize,
        end_index: usize,

        const This = @This();

        fn init(start_bucket: ?*BucketArrayUnmanaged(N, T).List.Node, end_index: usize) This {
            return This{ .current_bucket = start_bucket, .current_index = 0, .end_index = end_index };
        }

        pub fn next(this: *This) ?*T {
            if (this.current_index >= N) {
                this.current_bucket = this.current_bucket.?.next;
                this.current_index = 0;
            }

            const bucket = this.current_bucket orelse return null;

            if (bucket.next == null and this.current_index == this.end_index) {
                this.current_bucket = null;
                return null;
            }

            const n = &bucket.data[this.current_index];
            this.current_index += 1;

            return n;
        }
    };
}
